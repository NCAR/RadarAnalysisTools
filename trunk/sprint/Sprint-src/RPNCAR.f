      SUBROUTINE RPNCAR(NUFST,JPCK,ELSCAN,ICRTST,CTDBM,CTDBMXH,CTDBMXV,
     X     NTHRSH,TFIELD,TLIMITS,RFNAM,FNUM,P1,P2,P3,P4,P10,AZCOR,
     X     DASANG,ISIDE,USER_DEF_ORIGIN,RADAR_CHOSEN,VNYQUIST,IFD,
     X     IFD_RAYS)
C     
C     THIS SUBROUTINE READS ONE VOLUME SCAN IN RP3-7 FORMAT
C     AND OUTPUTS IT TO DISK IN A SIMPLIFIED FORMAT.
C     
C     NUFST - flag designating current input file status
C        NUFST = 0: NO END YET (NORMAL EXECUTION)
C        NUFST = 1: PAST ENDING TIME
C        NUFST = 2: END OF DATA
C        NUFST = 3: UNABLE TO INTERPOLATE VOLUME SCAN
C        NUFST = 4: END OF SWEEP FILES, BUT MAY HAVE ALREADY READ QUIT COMMAND.
C     
C     ICOPLANE - flag designating the type of radar scan and interpolation
C        ICOPLANE = 0: R,A,E                   -> Cartesian
C        ICOPLANE = 1: R,A,C                   -> COPLANE (angles of data)
C        ICOPLANE = 2: R,A,C                   -> COPLANE (users-specified angles)
C        ICOPLANE = 3: R,A,C                   -> Cartesian
C        ICOPLANE = 4: RHI                     -> Cartesian
C        ICOPLANE = 5: AIR (For, Aft, or Tail) -> Cartesian
C     
C     ATD/FOF Radar processor (3-7) significant housekeeping words:
C         4 - Year of the data (last two digits)
C         5 - Month of the data
C         6 - Day of the data
C         7 - Hour of the data
C         8 - Minute of the data
C         9 - Second of the data
C        10 - Current azimuth angle (degrees * CF)
C        11 - Current elevation angle (degrees * CF)
C        14 - Gate spacing (m)
C        15 - Number of range gates
C        20 - Primary wavelength PRF (Hz * 10)
C        21 - Primary wavelength (cm * 100)
C        22 - Sequential sweep number
C        26 - Scan mode: (0) Calibration, (1) PPI or sector, (2) Coplane,
C                        (3) RHI, (4) Vertical, (5) Target, (6) Manual, 
C                        (7) Idle, (8) Surveillance.
C        31 - Fixed angle (degrees * CF)
C        36 - Primary coplane baseline angle (degrees * CF)
C        61 - Beam transition flag (0 - in the scan, 1 - in transition)
C        62 - Data system identifier (RP3, 4, 5, 6, or 7)
C        63 - Radar system identifier (CP2, 3, 4 or "MH", 
C             with MH=4d48 hexadecimal)
C        68 - Number of parameters (fields) associated with each gate
C
C     ANGXAX = ANGLE OF X AXIS FROM TRUE NORTH (USUALLY 90.0)
C     BASANG = ANGLE OF Y AXIS FROM TRUE NORTH; USED IN COPLANE SCANS
C     DASANG = ANGLE OF Y AXIS FROM BASELINE OF RADARS; USED IN COPLANE SCANS
C        Values of BASANG and DASANG:
C           - Not used for non-coplane scans
C           - Same value when interpolating from coplane scans to coplane
C           - May be different when interpolating from coplane scans to xyz
C     
C     GDBEAMS - Beam counter incremented here, GDBEAMS .le. MRAYS.
C     BDBEAMS - Number of beams that have been thrown out.
C     ELTUS   - Acceptable departure of actual "fixed" angle from the nominal
C               fixed angle.  Beams with |Fixed - Actual| > ELTUS are tossed,
C               where Actual = (PPI) elevation angle, or (RHI) azimuth angle.
C               ELTUS is specified with the RESET command (see RNGFIL) and is
C               passed around with the /CRNG/ common block.
C     FXSTOL  - When using the fixed angle (ISWUSR = 'F'IXED) to delineate 
C               scans, all beams within FXSTOL of the first beam in the 
C               sequence belong within the same sweep regardless of sweep 
C               number.  When using the sweep number (ISWUSR = 'N'UMBER) 
C               to delineate scans, beams with the same sweep number are 
C               processed together and their angular positions are assigned 
C               to the nominal fixed angle, regardless of their actual position.
C               FXSTOL is specified with the PROCESS command (see PROFIL) and 
C               is passed around with the /CPRO/ common block.
C
C     Note: KDAY - user requested day to be processed (set in PROFIL)
C           JDAY - day of the first ray within the current volume scan
C           IDAY - day of the current ray of data of the current volume scan
C           If the current day changes from the 1st ray day, add 24 to hours.
C
C     KOUT contains a beams worth of information which is written to
C     Sprint's internal disk (fort.1) in WRRYDK where KOUT (KUNBUF) is 
C     first put into array KPKBUF, whose 1st value is the record length.
C        KOUT(  1)  - scaled integer azimuth (AZ*JRH6 = AZ*64)
C        KOUT(  2)  - scaled integer fixed angle (FXANG*JRH7 = FXANG*100)
C        KOUT(3-5)  - integer time (hour-min-sec) of beam
C        KOUT(6-7)  - scaling factors (JRH6, JRH7) = (64, 100)
C        KOUT(  8)  - number of range gates (NRG)
C        KOUT(9-10) - 1000*latitude/longitude of the radar 
C        KOUT(>10)  - contains NOF*(NRG field values)
C
      INCLUDE 'SPRINT.INC'
c      PARAMETER (MAXEL=150,NID=129+3*MAXEL)
c      PARAMETER (NIOB=85000,MAXIN=8500,MAXLEN=MAXIN/4)
c      PARAMETER (MAXRNG=1024,MAXFLD=16)
c      DATA LFTIM,JRH6,JRH7,IBAD /0,64,100,-32768/
c      PARAMETER (MAXSKP=27,MXCNT=500)

      DIMENSION MTFIEL(MAXFLD)
      DIMENSION JPCK(1),ELSCAN(1),IOVER(6),ISIDE(MAXFLD)
      DIMENSION TLIMITS(2,MAXFLD),THVAL(2,MAXRNG),ITHR(MAXFLD+1)
      DIMENSION CTDBM(MXCNT),CTDBMXH(MXCNT),CTDBMXV(MXCNT)
      DIMENSION VNYQUIST(MAXEL)

      CHARACTER*8 KRD(10),RFNAM,P10,NTM(MAXFLD)
      CHARACTER*8 CTEMP1,NAMFLD
      CHARACTER*2 NAMPRE
      CHARACTER*8 TFIELD(2,MAXFLD),THON(2),CFIELD,CTEMP2,ITM(MAXFLD,3)
      COMMON /IO/KPCK(NIOB),KOUT(MAXIN),IBUF(MAXIN),NBUF(MAXLEN,4),
     X     IZ8(17),ILSTREC
      COMMON /IDBLK/ID(NID)
      COMMON /FXTABL/ IFXTAB,ISKIP,IACCPT,FTABLE(MAXSKP),ITRAN
      
      COMMON /CFLD/NFLDS
      COMMON /CFLDC/ IFIELD(MAXFLD), INTINF(MAXFLD,3)
      CHARACTER*8 INTINF,IFIELD
      
      COMMON /CINP/IUN,ISKP,ORLAT,ORLON
      COMMON /CINPC/ NMRAD,ITAP,TAPE_UNIT
      CHARACTER*4 NMRAD
      CHARACTER*8 ITAP
      CHARACTER*8 TAPE_UNIT
      
      COMMON /UNITS/LUN,LSKP,FEETS
      COMMON /UNITSC/ IPROJ,LTAP,ISCI
      CHARACTER*4 IPROJ
      CHARACTER*8 LTAP,ISCI
      
      COMMON /CRNG/ELTUS,MNBEM,RUSR1,RUSR2,RG1,RG2,DRG,NRG,VNYQ,
     X     RNOTUS,DRGUS,CFAC1,CFAC2,CFAC3
      COMMON /CRNGC/ IRCFXL
      CHARACTER*8 IRCFXL
      
      COMMON /CPRO/KDAY,KBTIM,KETIM,IROV,ISWMTH,FXSTOL
      COMMON /CPROC/ IREORD(3)
      CHARACTER*1 IREORD
      
      INTEGER SWAPPING
      COMMON /BYTORD/ MBYTE,SWAPPING

      COMMON /SCRDSK/ LTMP
      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI,ILLE,ILLZ
      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X     ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      LOGICAL IELCHK,ILSCHK
      LOGICAL WRIT_ONCE
      CHARACTER*8 NFLDTMP,IFLDTMP
      CHARACTER*3 KOMM
      CHARACTER*8 INP,IOUT,MET,ICRT,IEMPTY,NAMTIM

      INTEGER CVMGP
      INTEGER FNUM
      INTEGER GDBEAMS,BDBEAMS

      DATA INP,IOUT,MET,ICRT/'INPUT','OUTPUT','INTERP','GRID'/
      DATA IEMPTY/'-9999'/
      DATA MFTOL/25/
C     
C     OVERFLOW COUNTER
C     
      DATA IOVER /6*0/

C     MXL   IS THE MAXIMUM ELEVATION*10+1 TO ALLOW IN THE MODAL SELECTION
C     MXSCN IS THE MAXIMUM EXPECTED NUMBER OF BEAMS/SCAN

      DATA MXL/901/
      DATA MXSCN/1000/
      DATA NAMTIM/'TIME'/

C     THE FOLLOWING IS THE DEGREES TO RADIANS CONVERSION

      DATA DTR /0.0174533/
C     
C     IN-LINE FUNCTIONS:
C        INORM: Normalize 16-bit 2s complement integers
C        INANG: Change scaled (x 182.04444) elevations in 350-360 range negative (0 TO -10)
C        CALCOP: Calculates coplane angle from elevation and azimuth angle
C        CALEL: Calculates elevation angle from fixed and azimuth angle.
C        CALHAZ: Calculate horizontal azimuth from azimuth in coplane and coplane angle.
C
      INORM(I)=CVMGP(I-65536,I,I-32768)
      INANG(I)=CVMGP(I-65536,I,I-63716)
      CALCOP(E,A)=ATAN((TAN(E*DTR)/ABS(SIN(A*DTR))))/DTR
      CALEL(F,A)=ATAN((TAN(F*DTR)*ABS(SIN(A*DTR))))/DTR
c-----CALHAZ(A,C)=ATAN(TAN(A*DTR)*COS(C*DTR))/DTR
C     
C     CALCULATE RADAR COORDINATES IN ROTATED (IF ROTATED) COORD. SYS.
C     
      IF (ANGXAX.NE.90.0) THEN
         DHETA=(ANGXAX-90.0)*DTR
         XORR=FLOAT(ID(47))/100.*COS(DHETA) - FLOAT(ID(48))/100.
     X        *SIN(DHETA)
         YORR=FLOAT(ID(47))/100.*SIN(DHETA) + FLOAT(ID(48))/100.
     X        *COS(DHETA)
         ZORR=FLOAT(ID(46))/1000.
      ELSE
         XORR=FLOAT(ID(47))/100.
         YORR=FLOAT(ID(48))/100.
         ZORR=FLOAT(ID(46))/1000.
      END IF
      
      IEOF    = 0
      IFLGBAS = 0
      BASANG  = ANGXAX-90.0
      SCALE   = 1.0/182.044444
      ELTOL   = ELTUS
      IDRGCHG = 0
      INRNG   = 0
C
C     Store the user-specified angle for +X-axis and restore ANGXAX
C     before leaving this routine.  ANGXAX is used to rotate the RHIs.
C
      ANGXAXT=ANGXAX
C     
C     CHECK TO SEE IF ALL COMMONS ARE FILLED - IF NOT STOP
C     
      IF (ITAP.NE.IEMPTY) GOTO 5
      PRINT 26,INP
      STOP
 5    CONTINUE
      IF (LTAP.NE.IEMPTY) GOTO 10
      PRINT 26,IOUT
      STOP
 10   CONTINUE
 20   CONTINUE
      IF (ISCI.NE.IEMPTY) GOTO 25
      PRINT 26,MET
      STOP
 25   CONTINUE
      IF (ICRTST.GE.0) GOTO 30
      PRINT 26,ICRT
      STOP
 26   FORMAT(5X,'+++  ',A8,' COMMAND MUST APPEAR BEFORE THE PROCESS',
     X     ' COMMAND  +++')
C     
C     OTHER CHECKS GO HERE
C     
 30   CONTINUE
      NFLINP=NFLDS
      IF(IFIELD(NFLDS).EQ.NAMTIM) NFLINP=NFLINP-1

 200  CONTINUE
C     
C     200: Top of the "FIND VOLUME" loop
C          Initialize threshold counters
C
      DO I=1,MAXFLD+1
         ITHR(I)=0
      END DO
C     
C     DETERMINE THE METHOD OF FIXED ANGLE COMPUTATION
C        IELT: (0) Actual, (1) Mode, (2) Mean
C     
      WRITE (CTEMP1,43)IRCFXL
 43   FORMAT(A8)
      READ (CTEMP1,45)I,GNEL
 45   FORMAT(A2,1X,F5.0)

      IF(CTEMP1(1:2).EQ.'MD') THEN
C     Use MODE of the elevation angles
         IELT=1
         IGNEL=AMAX1(GNEL,0.0)
         IELCHK=.TRUE.
      ELSE IF(CTEMP1(1:2).EQ.'MN') THEN
C    Use MEAN of the elevation angles
         IELT=2
         IGNEL=AMAX1(GNEL,0.0)
         IELCHK=.TRUE.
      ELSE
C     Use FIXED ANGLE in file
         IELT=0
         IGNEL=0
         IELCHK=.FALSE.
      END IF
      print *,'FF: ielchk,ilschk,eltol,fxstol=',
     +     ielchk,ilschk,eltol,fxstol
C     
C     CHECK TO MAKE SURE USER IS NOT REQUESTING INTERP. TO DEFAULT
C     COPLANE ANGLES AND REQUESTING A REDEF. OF FIXED ANGLES
C     
      IF (ICOPLANE.EQ.1 .AND. IELCHK) THEN
         WRITE(*,*)'***CANNOT INTERPOLATE TO DEFAULT COPLANE ANGLES',
     X        ' OF DATA AND REDEFINE FIXED ANGLE AS MEAN OR MODE'
         STOP
      END IF
C
C     Initialize (IFLG=-9) Unit LTMP=1 (fort.1) as SPRINT's internal disk file
C
      LTMP=1
      IFLG=-9
      WRITE(LTMP)LTMP
      REWIND LTMP
      CALL WRRYDK(KPCK,KOUT,INST,LTMP,IFLG,0)
c     subroutine WRRYDK(KPKBUF,KUNBUF,NST,IUN,IFLG,NLEN)
c-----debug (ljm) - eor/uio: "off end of record" in WRRYDK at READ(IUN)
c     Fix that worked was to restore RDRY common block in RDRYDK.
      jst=inst
      mlen=0
      write(7,*)'FFint: unit,jst,iflg,len,plen=',
     +     ltmp,jst,iflg,mlen,ilstrec
c-----debug (ljm)
C
C     Read in the first beam and check so that it can tested
C     to see if it satisfies the conditions requested by the user.
C
      CALL NWRAY(NWDS,MBYTE,NST,SWAPPING)
      MRAYS = 1
      IPREC = 1
      NAZZ  = 1
      IF(IFD.EQ.1)THEN
         IF(MOD(MRAYS,IFD_RAYS).EQ.0)THEN
            write(7,*)'First FF record in this file'
            CALL FFDUMP(IBUF,NWDS,MAXIN,NST,SCALE,IPREC,NAZZ)
         END IF
      END IF
      IF (NST.EQ.3) THEN
         PRINT 51,ITAP,IUN
 51      FORMAT (5X,'**END OF INFORMATION ON TAPE ',A6,' UNIT ',I3)
         NUFST=2
         RETURN
      END IF  
C     
C     CHECK IF VOLUME IS THE ONE REQUESTED AND IS A SCAN MODE THAT CAN BE 
C     INTERPOLATED.  Acceptable modes are: (1) PPI or sector, (2) Coplane, 
C                                          (3) RHI, and (8) Surveillance.
C     IFUT=IBUF(32)
C     CALL SHILBL(IFUT,1)
      LFTIM=0

C     MARK BRADFORD PATCH TO REMOVE CONVERSION TO MDT
C     IF (IFUT.EQ.'UT') LFTIM=-6
C
      IHR=IBUF(7)+LFTIM
      IF (IHR.LT.0) IHR=IHR+24
      IFTIM=10000*IHR+100*IBUF(8)+IBUF(9)
      JDAY=10000*IBUF(4)+100*IBUF(5)+IBUF(6)
      IF (KETIM.NE.999999) THEN
C     Position by date and time; else any volume is ok
         IF (JDAY.LT.KDAY) THEN
            PRINT 57, JDAY,IFTIM
 57         FORMAT(' +++  VOLUME SKIPPED  -  DAY: ',I6,
     X           5X,'BEGINNING TIME: ',I6,'  +++')
            CALL SKPVOL(IUN,1)
C     
C     Date in file .lt. requested date - go to 200 and seek next volume
C     
            GOTO 200
         ELSE IF (JDAY.GT.KDAY) THEN
C     Up time by 24 hours 
            IFTIM=IFTIM+240000*(JDAY-KDAY)
         END IF
         IF (IFTIM.LT.KBTIM) THEN
            PRINT 57, JDAY,IFTIM
            CALL SKPVOL(IUN,1)
C     
C     Time in file .lt. requested time - go to 200 and seek next volume
C     
            GOTO 200
         ELSE IF (IFTIM.GT.KETIM) THEN
            PRINT 61, IUN,IFTIM,KETIM
 61         FORMAT(/1X,'+++  UNIT: ',I2,5X,'INITIAL TIME IN FILE: ',I6,
     X           '  IS PAST THE REQUESTED ENDING TIME TO PROCESS: ',I6,
     X           '  +++'/)
            NUFST=1
            BACKSPACE IUN
C     
C     CAN'T FIND REQUESTED VOLUME IN THIS FILE
C     
            RETURN
         ENDIF
      END IF

C     Found the requested volume.  Check if this scan mode can be interpolated.
C
C     IBUF(26) - Scan mode: (0) Calibration, (1) PPI or sector, (2) Coplane,
C                           (3) RHI, (4) Vertical, (5) Target, (6) Manual, 
C                           (7) Idle, (8) Surveillance.
c-----debug (ljm)
      write(7,*)'FFvol: Beginning-of-volume'
      writ_once = .false.
c-----debug (ljm)

      JDAY=10000*IBUF(4)+100*IBUF(5)+IBUF(6)
      PRINT 63,JDAY,IFTIM
 63   FORMAT (//100('+')//
     X     8X,'VOLUME FOUND   -   DAY : ',I6.6,
     X     8X,'BEGINNING TIME : ',I6)
      IF(IBUF(26).NE.1 .AND. IBUF(26).NE.2 .AND. 
     X   IBUF(26).NE.3 .AND. IBUF(26).NE.8) THEN
         PRINT 65,IBUF(26)
 65      FORMAT (5X,'***SCAN MODE ',I1,' CANNOT BE INTERPOLATED ',
     X        '- VOLUME DISCARDED***')
         CALL SKPVOL(IUN,1)
C     
C     Requested date/time okay, but wrong scan mode - go to 200 and seek next volume
C     
         GOTO 200
      ENDIF
C
C     If necessary, reassign the value for ICOPLANE.
C
      IF (ICOPLANE.EQ.1 .OR. ICOPLANE.EQ.2) THEN
C
C     Interpolation from Coplane scans to Coplane grid
C
         IF (IBUF(26).NE.2) THEN
            WRITE(*,71)
 71         FORMAT('***WARNING. VOLUME NOT SCANNED IN COPLANES.',
     X           ' INTERPOLATING ANYWAY***')
c---------------------------------------------------------------------
c 71         FORMAT(' ***REQUESTED COPLANE INTERP--VOLUME IS NOT ',
c     X           'COPLANE...SKIPPED DAY: ',I6,' BEGINNING TIME: ',I6)
c            CALL SKPVOL(IUN,1)
c            GOTO 200
c---------------------------------------------------------------------
         END IF
      ELSE
         IF (IBUF(26).EQ.2) THEN
C
C     Interpolation from Coplane scans to Cartesian grid
C
            ICOPLANE=3
         ELSE IF (IBUF(26).EQ.3) THEN
C
C     Interpolation from RHI scans to Cartesian grid
C
            ICOPLANE=4
         ELSE
C
C     Interpolation from PPI (SECTOR) or SURVEILLANCE scans to Cartesian grid
C
            ICOPLANE=0
         END IF
      END IF
C
C     Print information about the type of interpolation
C
      IF (ICOPLANE.EQ.0) THEN
         WRITE(*,75)
 75      FORMAT(5X,'+++GRIDDING DATA FROM PPI TO 3-D CARTESIAN+++')
      ELSE IF (ICOPLANE.EQ.1 .OR. ICOPLANE.EQ.2) THEN
         DASANG=BASANG
         WRITE(*,77)
 77      FORMAT(5X,'+++GRIDDING DATA FROM COPLANE TO REGULAR COPLANE ',
     X        'GRID+++')
      ELSE IF (ICOPLANE.EQ.3) THEN
         IF (DASANG.EQ.-999.0) DASANG=IBUF(36)*SCALE
         BASANG=DASANG
         WRITE(*,79)
 79      FORMAT(5X,'+++GRIDDING DATA FROM COPLANE TO 3-D CARTESIAN+++')
      ELSE IF (ICOPLANE.EQ.4) THEN
         WRITE(*,81)
 81      FORMAT(5X,'+++GRIDDING DATA FROM RHI TO 3-D CARTESIAN+++')
      END IF

      IF (ICOPLANE.EQ.4 .AND. ANGXAX.NE.90.0) THEN
C
C     DUE TO CONTORTED WAY THAT RHIs HAVE TO BE INTERPOLATED,
C     X-AXIS ROTATIONS NEED TO BE HANDLED VIA AZIMUTH CORRECTION.
C         
         AZCORT=AZCOR
         AZCOR=AZCOR+(90.-ANGXAX)
         ANGXAXT=ANGXAX
         ANGXAX=90.
         WRITE(*,85)AZCOR,ANGXAXT
 85      FORMAT(5X,'+++RHI AZIMUTHS WILL BE ROTATED ',
     X        F6.2,' DEGRESS SINCE AZIMUTH +X-AXIS SPECIFIED AS ',F6.2,
     X        ' INSTEAD OF 90 DEG+++')
      END IF
C
C     CHECK TO SEE IF THE USER HAS REQUESTED DBMXVUF TO BE INTERPOLATED.
C     IF SO, DO SOME SPECIAL PROCESSING.
C
      CALL VERTCHK(IFIELD, TFIELD, NFLINP, NTHRSH, IBUF, NDBMXH)
C     
C     FOUND VOLUME - START PROCESSING IT
C     
c-----debug (ljm)
      write(7,*)'FFvol: Volume found CALL INITVOL'
      writ_once = .false.
c-----debug (ljm)

      CALL INITVOL(IPROJ)
      INRNG   = 0
      IDRGCHG = 0
      MFBM    = 0
      ID(4)   = IHR
      IVOL    = 0
      ISWP    = 0
      NSWPS   = 0
      NRAYS   = 0
      GDBEAMS = 0
      BDBEAMS = 0
      MRAYS   = 0
      NAZZ    = 0
      IPREC   = 0
C     
C     DETERMINE IF USER IS SETTING NYQUIST VELOCITY
C     OR USING THE VALUE WRITTEN IN THE FILE.
C     
      IF (VNYQ.EQ.0.0) THEN
         VNY=IBUF(21)*IBUF(20)*.0000025
      ELSE
         VNY=VNYQ
      END IF
      VNYQSWP=VNY
C     
C     NORMALIZE ELEVATION/CHECK FOR NEGATIVES
C     
C     IBUF(31)=INORM(IBUF(31))
      IF (ICOPLANE.NE.4) IBUF(31)=INANG(IBUF(31))
      ELSAV=IBUF(31)*SCALE
C      IPTR=129
      IPTR=IPTR_INT
C      IF (IPTR.GT.129 .AND. ABS((ELSAV+AZCOR)-ID(IPTR-3)/
      IF (IPTR.GT.IPTR_INT .AND. ABS((ELSAV+AZCOR)-ID(IPTR-3)/
     X     REAL(ID(44))).GT.180.0) THEN
         ELSAV=ELSAV+360.0
      END IF
      NGTSOLD=IBUF(15)
      DIR=0.0
C
C     SCANGL - VALUE OF THE ANGLE [AZIMUTH (10) or ELEVATION (11)]
C              THAT CHANGES DURING THE SCAN (PPI or RHI).
C
      IF (ICOPLANE.LT.4) THEN
C     
C     FOR PPI and COPLANE CASES, SCANGL IS AZIMUTH ANGLE [IBUF(10)].
C     
         SCANGL=IBUF(10)*SCALE + AZCOR
         IF (SCANGL.GT.360.0) SCANGL=SCANGL-360.0
         IF (SCANGL.LT.0.0) SCANGL=SCANGL+360.0
      ELSE IF (ICOPLANE.EQ.4) THEN
C     
C     FOR RHI CASE, SCANGL IS ELEVATION ANGLE [IBUF(11)].  ROLES OF
C     ELEVATION AND AZIMUTH ANGLES WILL REVERSED.  AN "AZIMUTH" ANGLE
C     IN THE SCAN PLANE WILL BE AZC = 90 - ELEV.
C     
         SCANGL=IBUF(11)*SCALE
      END IF

      BEGAZ  = SCANGL
      IBEGRG = ID(31)*1000+ID(32)
      BDBEAMS= 0
      ELMAX  = 0.0
      AZMAX  = 0.0
      ELSUM  = 0.0
      ELMIN  = 1000.0
      AZMIN  = 1000.0
      IFIRST = 1
      CALL CONFLD(ELSCAN,MXSCN+MXL,0.0)

      IF (ICOPLANE.EQ.0) THEN
         PRINT 91
      ELSE IF (ICOPLANE.EQ.1 .OR. 
     X         ICOPLANE.EQ.2 .OR.
     X         ICOPLANE.EQ.3) THEN 
         PRINT 93
      ELSE IF (ICOPLANE.EQ.4) THEN
         PRINT 95
      END IF
 91   FORMAT (//6X,'SCAN',18X,'ELEVATION',29X,'AZIMUTH',14X,'SPACING',
     X     18X,'BEAMS'/4X,'NO',3X,'DIR',9X,'FIXED',6X,'MIN',6X,'MAX',
     X     5X,'MEAN',10X,'BEG',7X,'END',6X,'MIN',6X,'MAX',
     X     5X,'MEAN',7X,'GOOD',3X,'BAD',3X,'VNYQ')
 93   FORMAT (//6X,'SCAN',18X,'COPLANE',29X,'AZIMUTH',14X,'SPACING',
     X     18X,'BEAMS'/4X,'NO',3X,'DIR',9X,'FIXED',6X,'MIN',6X,'MAX',
     X     5X,'MEAN',10X,'BEG',7X,'END',6X,'MIN',6X,'MAX',
     X     5X,'MEAN',7X,'GOOD',3X,'BAD',3X,'VNYQ')
 95   FORMAT (//6X,'SCAN',18X,'AZIMUTH',29X,'ELEVATION',14X,'SPACING',
     X     18X,'BEAMS'/4X,'NO',3X,'DIR',9X,'FIXED',6X,'MIN',6X,'MAX',
     X     5X,'MEAN',10X,'BEG',7X,'END',6X,'MIN',6X,'MAX',
     X     5X,'MEAN',7X,'GOOD',3X,'BAD',3X,'VNYQ')
      IALREAD=1
C     
C     CHECK IF USER WANTS SCANS (SWEEPS) DISCARDED (KEPT) ACCORDING 
C     TO USER-SUPPLIED LIST OF SWEEP NUMBERS TO SKIP (PROCESS).
C     LIST IS SET WITH FXTABLE COMMAND (See FXTABLE routine).
C     
      ISKIPFX=0
      IF (ISKIP.EQ.1 .AND. IFXTAB.EQ.1) THEN
         NSWP1=NSWPS+1
         DO ISK=1,MAXSKP
            IF (FTABLE(ISK).EQ.REAL(NSWP1)) THEN
               ISKIPFX=1
               GOTO 100
            END IF
         END DO
 100     CONTINUE
      ELSE IF (IACCPT.EQ.1 .AND. IFXTAB.EQ.1) THEN
         NSWP1=NSWPS+1
         DO I=1,MAXSKP
            IF (REAL(NSWP1).EQ.FTABLE(I)) GOTO 105
         END DO
         ISKIPFX=1
 105     CONTINUE
      END IF
c-----debug (ljm) 
      write(7,*)
     + 'UF (bov): begin loop over beams, ivol,iswp=',ivol,iswp
c     Check dimensions of Sprint disk arrays (KPCK, KOUT)
c     See also WRRYDK (KPKBUF, KUNBUF).
c     Need enough to write a beams worth of data to disk.
c
      if(.not.(writ_once))then
         write(7,*)'  '
         fxang = ibuf(31)*scale
         el    = ibuf(11)*scale
         az    = ibuf(10)*scale
         write(7,*)
     +        'FF (bov): sw-fxea,maxin=',iswp,fxang,el,az,maxin
         writ_once = .true.
      end if
c-----debug (ljm)
C-------------------------------------+
C     Top of main loop over beams     | 
C-------------------------------------+
      DO 1050 ILP=1,1000000
         IF (ILP.EQ.1000000) THEN
            WRITE(6,*)'ERROR-TOO MANY RAYS IN RPNCAR'
            STOP
         END IF
         IF (IALREAD.EQ.1) THEN
            IALREAD=0
         ELSE
            CALL NWRAY(NWDS,MBYTE,NST,SWAPPING)
            MRAYS=MRAYS+1
            IPREC=IPREC+1
            NAZZ=NAZZ+1
            IF(IFD.EQ.1)THEN
               IF(MOD(MRAYS,IFD_RAYS).EQ.0)THEN
                  CALL FFDUMP(IBUF,NWDS,MAXIN,NST,SCALE,IPREC,NAZZ)
               END IF
            END IF
         END IF
C     
C     HERE WE BRANCH BASED ON THE STATUS OF THE READ
C     
 120     CONTINUE
         IF (NST.EQ.0 .OR. IJ.EQ.1) THEN
            IF (IJ.EQ.1) IJ=0
            IF (NST.EQ.0) IEOF=0

C     IF BEAM IS FLAGGED AS A TRANSITION BEAM AND USER WANTS SUCH BEAMS
C     DISCARDED, DO IT!
C            
            IF (ITRAN.EQ.1 .AND. IBUF(61).EQ.1) GOTO 1050
C     
C     ***GOOD READ - CONSTRUCT RAY HEADER
C     
            KOUT(6)=JRH6
            KOUT(7)=JRH7
C     
C     CHECK IF THIS IS THE FIRST SWEEP TO BE PROCESSED; IF SO GET RANGE INFO 
C     FROM IT
C     
            IF (ISKIPFX.EQ.0 .AND. IFIRST.EQ.1) THEN
               IFIRST=0
               CALL INITVOL(IPROJ)
            END IF
C     
C     SEE IF THIS RAY SHOULD BE THROWN OUT BECAUSE IT'S PART OF AN UNWANTED
C     ELEVATION SCAN
C     
            IF (ISKIPFX.EQ.1) THEN
               ELCUR=ELSAV
               IF (ELCUR.GT.360.0) ELCUR=ELCUR-360.0
               IF ((ABS(IBUF(31)*SCALE-ELCUR).LE.FXSTOL) .AND.
     X              NGTSOLD.EQ.IBUF(15)) GOTO 1050
            END IF
C     
C     Calculate azimuth angle and store scaled integer value in KOUT(1)
C     Calculation of azimuth angle depends on type of scan and output grid.
C     
            IF (ICOPLANE.EQ.1 .OR. ICOPLANE.EQ.2) THEN
C     
C     Coplane scan: convert horizontal azimuth to azimuth in the coplane
C        interpolate to original coplane angles of data or user-specified
C     
               AZZ=IBUF(10)*SCALE + AZCOR
               AZZ=AZZ-BASANG
               IF (AZZ.LT.0.0) AZZ=AZZ+360.0
               IF (AZZ.GT.360.0) AZZ=AZZ-360.0
               IF (AZZ.GT.180.0 .OR. AZZ.LT.0.0) THEN
C     
C     Sector scan is on other side of baseline - print warning message
C     
                  WRITE(*,123)AZZ
 123              FORMAT('***BEAM ON WRONG SIDE OF BASELINE.',
     X                 ' HOR. AZIMUTH=',F8.2,' ---BEAM DISCARDED***')
                  BDBEAMS=BDBEAMS+1
                  GOTO 1050
               END IF
               COP=IBUF(31)*SCALE
               IF (AZZ.EQ.90.0) THEN
                  AZC=90.0
               ELSE IF (COP.LT.90.0) THEN
                  AZC=(ATAN2(TAN(AZZ*DTR),COS(COP*DTR))/DTR)
                  IF (AZC.LT.0) AZC=AZC+180.0
               ELSE
                  WRITE(*,*)'***BAD ANGLES IN RPNCAR***'
                  STOP
               END IF
               KOUT(1)=NINT(AZC*FLOAT(JRH6))

            ELSE IF (ICOPLANE.EQ.0 .OR. ICOPLANE.EQ.3) THEN
C     
C     Constant elevation or coplane scan: interpolation to cartesian (x,y,z)
C     
               AZZ=(IBUF(10)*SCALE + AZCOR)
               IF (AZZ.GT.360.0) AZZ=AZZ-360.0
               IF (AZZ.LT.0.0) AZZ=AZZ+360.0
               AZTMP=(IBUF(10)*SCALE + AZCOR)
               IF (AZTMP.GT.360.0) AZTMP=AZTMP-360.0
               IF (AZTMP.LT.0.0)   AZTMP=AZTMP+360.0
               KOUT(1)=NINT((AZTMP)*FLOAT(JRH6))

            ELSE IF (ICOPLANE.EQ.4) THEN
C
C     Constant azimuth scan (RHI) : interpolation to cartesian (x,y,z)
C     AZC - equivalent azimuth angle in vertical scan plane.  Elevation
C     is measured counter-clockwise whereas AZC is measured clockwise.  
C     Note: These "negative azimuth" angle beams must be discarded in 
C           order for the interpolation to work.  Negative azimuths are
C           not allowed in the TRPVOL interpolation code.
C     
               AZC=90.0 - IBUF(11)*SCALE
               IF (AZC.LT.0.05 .OR. AZC.GT.89.95) THEN
                  BDBEAMS=BDBEAMS+1
                  GOTO 1050
               END IF
               AZ=IBUF(10)*SCALE + AZCOR
               KOUT(1)=NINT(AZ*FLOAT(JRH6))
               AZZ=AZ
            ELSE
               WRITE(*,*)'***INVALID INTERPOLATION MODE IN RPNCAR***'
               STOP
            END IF
C     
C     NORMALIZE ELEVATION ANGLE and CHECK FOR NEGATIVES
C        Store information in KOUT(2-5)
C
C           IBUF(11)=INORM(IBUF(11))
            IBUF(11)=INANG(IBUF(11))
            KOUT(2)=IBUF(11)*SCALE*KOUT(7)

            IDAY=10000*IBUF(4)+100*IBUF(5)+IBUF(6)

            KOUT(3)=IBUF(7)+LFTIM
            IF (KOUT(3).LT.0) KOUT(3)=KOUT(3)+24

C     Increase the hour by 24 to allow for crossing 00 hour within a volume scan;
C     otherwise, the TIME field will be incorrect.  Only one day-change allowed.
C           JDAY - day of the first ray within the current volume scan
C           IDAY - day of current ray within the current volume scan
C
            IF (IDAY.NE.JDAY)THEN
               KOUT(3)=KOUT(3)+24
            END IF
c            print *,'RPNCAR: jday,iday,ihr=',jday,iday,kout(3)
            KOUT(4)=IBUF(8)
            KOUT(5)=IBUF(9)
            ID(7)=KOUT(3)
            ID(8)=IBUF(8)
            ID(9)=IBUF(9)
C     
C     PROCESS SWEEP - KEY ON FIXED ANGLE OR NUMBER OF GATES
C     
            ELCUR=ELSAV
            IF (ELCUR.GT.360.0) ELCUR=ELCUR-360.0
            IF (ABS(ELCUR-(IBUF(31)*SCALE)).GT.FXSTOL .OR.
     X           NGTSOLD.NE.IBUF(15))
     X           THEN
C
C     A NEW SWEEP BEEN DETECTED - WRAP UP PREVIOUS SWEEP
C     
c--------------debug (ljm)
               if(iswmth.eq.0)then
                  write(7,*)'FF(EOS) sweep=',iswmth,ibuf(10),iswp
               else
                  write(7,*)'FF(EOS) fixed=',iswmth,elcur,elold,fxstol
               end if
c--------------debug (ljm)
               IF (GDBEAMS.LT.MNBEM .AND. IFXTAB.NE.1) THEN
c                  CALL WRRYDK(KPCK,KOUT,INST,LTMP,-9,0)
                  IF (GDBEAMS.GT.0) THEN
                     IFLG=NSWPS+11
                     CALL WRRYDK(KPCK,KOUT,INST,LTMP,IFLG,GDBEAMS)
                     WRITE(*,129)NSWPS+1,GDBEAMS
 129                 FORMAT(5X,'+++SWEEP ',I3,' DISCARDED. ONLY HAD',
     X                    I4,' BEAMS+++')
                  NRAYS=NRAYS-GDBEAMS
                  END IF
               ELSE
                  ELFIX=ELSAV
C     
C     RE-COMPUTE FIXED ANGLE IF REQUESTED
C     
                  IF(IELCHK) THEN
                     I1=IGNEL+1
                     I2=GDBEAMS-IGNEL
                     IF(I2.LT.I1) THEN
                        I1=GDBEAMS/2+1
                        I2=I1
                     END IF
                     SUM=0.0
                     DO 140 I=I1,I2
                        SUM=SUM+ELSCAN(I)
                        J=NINT(ELSCAN(I)*10.0)+1
                        IF(J.LT.1.OR.J.GT.MXL) GO TO 140
                        ELSCAN(MXSCN+J)=ELSCAN(MXSCN+J)+1.0
 140                 CONTINUE
                     IF(IELT.EQ.1) THEN
                        MODE=0
                        ELMOD=0.0
                        DO 142 I=1,MXL
                           IF(ELSCAN(MXSCN+I).LE.ELMOD) GO TO 142
                           MODE=I
                           ELMOD=ELSCAN(MXSCN+I)
 142                    CONTINUE
                        IF(MODE.NE.0) ELFIX=(MODE-1)*0.1
                     ELSE IF (IELT.EQ.2) THEN
                        ELFIX=SUM/(I2-I1+1)
                     END IF
                  END IF
                  ID(IPTR)=NINT(ELFIX*ID(44)) 
                  IF (ICOPLANE.EQ.4 .AND. .NOT.IELCHK .AND. 
     X                 AZCOR.NE.0.0) THEN
C
C     ADJUST FIXED ANGLE BY AZIMUTH CORRECTION TOO FOR RHIS
C
                     ELFIX=ELFIX+AZCOR
                     ID(IPTR)=NINT(ELFIX*ID(44))
                  END IF
                  ID(IPTR+1)=SIGN(1.0,DIR)
C
C     REVERSE DIRECTION OF SCAN FOR RHI SCANS
C
                  ELTMP=ELFIX
                  IF (ELTMP.GT.360.) ELTMP=ELTMP-360.
                  IF (ICOPLANE.EQ.4 .AND. ((ELTMP.LT.45.0 .AND.
     X                 ELTMP.GE.0) .OR. (ELTMP.LT.315. .AND.
     X                 ELTMP.GE.225.0) .OR. (ELTMP.LT.405 .AND.
     X                 ELTMP.GE.315))) ID(IPTR+1)=-ID(IPTR+1)
                  ID(IPTR+2)=NRAYS
                  NSWPS=NSWPS+1
                  IF (NSWPS.GT.MAXEL) THEN
                     PRINT 157,MAXEL
 157                 FORMAT(/,5X,'+++ MAX. NUMBER OF SWEEPS IS ',I3,
     X                    ' +++')
                     STOP 157
                  ENDIF
C
C     AVOID DIVISIONS BY ZERO IF LAST SWEEP WAS DISCARDED
C
                  IF (ISKIPFX.EQ.0) THEN
                     IF (GDBEAMS.GE.2) THEN
                        DELAZ=ABS(DIR)/(GDBEAMS-1)
                        DELEL=ELSUM/GDBEAMS
                     ELSE
                        DELAZ=0.0
                        DELEL=0.0
                     END IF
                  ELSE
                     DELAZ=0.0
                     DELEL=0.0
                  END IF
                  IF(ICOPLANE.EQ.4)THEN
                     FXSAV=ELSAV+AZCOR
                  ELSE
                     FXSAV=ELSAV
                  END IF
                  IF(IFD.EQ.1)THEN
                     WRITE(7,159)NSWPS,ID(IPTR+1),FXSAV,ELMIN,ELMAX,
     X                    DELEL,BEGAZ,SCANGL,AZMIN,AZMAX,DELAZ,GDBEAMS,
     X                    BDBEAMS,VNYQSWP
                  END IF
                  PRINT 159,NSWPS,ID(IPTR+1),FXSAV,ELMIN,ELMAX,DELEL, 
     X                 BEGAZ,SCANGL,AZMIN,AZMAX,DELAZ,GDBEAMS,BDBEAMS,
     X                 VNYQSWP
 159              FORMAT(2(4X,I2),8X,4(F6.2,3X),4X,2(F6.1,4X),
     X                 3(F6.2,3X),I7,I6,F7.2)
                  VNYQUIST(NSWPS)=VNYQSWP
                  NAZZ=0
                  IPTR=IPTR+3
C     
C     CHECK TO SEE IF NEXT ELEVATION SWEEP IS TO BE SKIPPED AT USER'S REQUEST
C     ISKIPFX=0  ==>  DON'T SKIP NEXT SWEEP
C     ISKIPFX=1  ==>  SKIP NEXT SWEEP
C     
                  ISKIPFX=0
                  IF (ISKIP.EQ.1 .AND. IFXTAB.EQ.1) THEN
                     NSWP1=NSWPS+1
                     DO ISK=1,MAXSKP
                        IF (FTABLE(ISK).EQ.REAL(NSWP1)) THEN
                           ISKIPFX=1
                           GOTO 160
                        END IF
                     END DO
 160                 CONTINUE
                  ELSE IF (IACCPT.EQ.1 .AND. IFXTAB.EQ.1) THEN
                     NSWP1=NSWPS+1
                     DO I=1,MAXSKP
                        IF (REAL(NSWP1).EQ.FTABLE(I)) GOTO 170
                     END DO
                     ISKIPFX=1
 170                 CONTINUE
                  END IF
               ENDIF
C     
C     GO DO END OF VOLUME PROCESSING
C     
               IF (NST.NE.0) GOTO 2000
C     
C     INITIALIZE QUANTITIES FOR NEXT SWEEP SINCE THE FIRST BEAM HAS ALREADY BEEN READ
C     
               GDBEAMS=0
               BDBEAMS=0
               DIR=0.0
               ELMAX=0.0
               AZMAX=0.0
               ELSUM=0.0
               ELMIN=1000.0
               AZMIN=1000.0

               IF (ICOPLANE.LT.4) THEN
C
C     All PPI/SUR (R,A,E) and COPLANE (R,A,C) scans --> Cartesian or coplane
C
                  SCANGL=IBUF(10)*SCALE + AZCOR
                  IF (SCANGL.GT.360.0) SCANGL=SCANGL-360.0
                  IF (SCANGL.LT.0.0) SCANGL=SCANGL+360.0
               ELSE IF (ICOPLANE.EQ.4) THEN
C
C     RHI scans --> Cartesian
C     
                  SCANGL=IBUF(11)*SCALE
                  IF (SCANGL.LT.0.0 .OR. SCANGL.GT.90.0) THEN
                     WRITE(*,*)'***BAD ANGLE IN RPNCAR***'
                     STOP
                  END IF
               END IF
               BEGAZ=SCANGL
               CALL CONFLD(ELSCAN,MXSCN+MXL,0.0)
C              ISWP=IBUF(10)
C     
C     NORMALIZE ELEVATION and CHECK FOR NEGATIVES
C     
C              IBUF(31)=INORM(IBUF(31))
               IF (ICOPLANE.NE.4) IBUF(31)=INANG(IBUF(31))
               ELSAV=IBUF(31)*SCALE
c               IF (IPTR.GT.129 .AND. ABS((ELSAV+AZCOR)-ID(IPTR-3)/
               IF (IPTR.GT.IPTR_INT .AND. ABS((ELSAV+AZCOR)-ID(IPTR-3)/
     X              REAL(ID(44))).GT.180.0) ELSAV=ELSAV+360.0
               NGTSOLD=IBUF(15)
            END IF
C     
C     PROCESS BEAM
C     
            IF(GDBEAMS.GE.MXSCN) THEN
C     
C     TOO MANY BEAMS IN THE SWEEP (NON-FATAL)
C     
               BDBEAMS=BDBEAMS+1
               PRINT 185, GDBEAMS
 185           FORMAT(5X,'*** TOO MANY BEAMS/SCAN  (',I5,' MAX )  ',
     X              '--RAY DISCARDED ***')
C     
C     GO GET ANOTHER BEAM ==> REST OF BEAMS IN CURRENT SWEEP WILL BE DISCARDED
C     
               GO TO 1050
            END IF
C     
C     PROCESS SWEEP 
C
            IF(.NOT.IELCHK) THEN
               THETA=IBUF(11)*SCALE
               PHI=IBUF(10)*SCALE + AZCOR - (ANGXAX-90.)
               IF (PHI.LT.0.0) PHI=PHI+360.0
               IF (PHI.GT.360.0) PHI=PHI-360.0
               IF (THETA.EQ.90.0) THEN
                  WRITE(*,*)'***BAD ANGLES IN RPNCAR***'
                  STOP
               END IF 
C     
C     ELANG IS THE ELEV ANGLE CALCULATED FROM THE FIXED AND AZIM ANGLES
C        IBUF(10) - Scaled integer azimuth ( x 64)
C        IBUF(11) - Scaled integer elevation ( x 64)
C        IBUF(31) - Scaled integer fixed angle ( x 64)
C     
               IF (ICOPLANE.EQ.1 .OR. ICOPLANE.EQ.2) THEN
C
C     Coplane scans (R,A,C) --> COPLANE
C
                  ELANG=CALEL(IBUF(31)*SCALE,PHI)
                  DASANG=BASANG
                  IF (DASANG.LT.0.0) DASANG=DASANG+360.0
                  IF (ELANG.LT.0.0 .OR. ELANG.GT.90.0) THEN
                     WRITE(*,*)'***INVALID ELEV ANGLE IN UFNCAR***'
                     STOP
                  END IF
               ELSE IF (ICOPLANE.EQ.3) THEN
C
C     Coplane scans (R,A,C) --> CARTESIAN
C
                  BASANG=DASANG
                  PHI = IBUF(10)*SCALE + AZCOR - BASANG
                  IF (PHI.LT.0.0) PHI=PHI+360.0
                  IF (PHI.GT.360.0) PHI=PHI-360.0
                  ELANG = CALEL(IBUF(31)*SCALE,PHI)
                  IF (ELANG.LT.0.0 .OR. ELANG.GT.90.0) THEN
                     WRITE(*,*)'***INVALID ELEV ANGLE IN UFNCAR***'
                     STOP
                  END IF
               ELSE IF (ICOPLANE.EQ.4) THEN
C
C     RHI scans --> CARTESIAN
C
                  ELANG=IBUF(10)*SCALE + AZCOR
               END IF
C     
C     Check tolerance between nominal (FXANG) and actual fixed angle
C        [PPI - IBUF(11) or RHI - IBUF (10)]
C        10 - Current azimuth angle (degrees * CF)
C        11 - Current elevation angle (degrees * CF)
C        31 - Fixed angle (degrees * CF)
C     
               IF (ICOPLANE.EQ.0) THEN
                  IF (ABS((IBUF(31)-IBUF(11))*SCALE).GT.ELTOL) THEN
                     BDBEAMS=BDBEAMS+1
C     
C     GO GRAB ANOTHER BEAM
C     
                     GOTO 1050
                  END IF
               ELSE IF (ICOPLANE.GE.1 .AND. ICOPLANE.LE.3) THEN
                  IF (ABS((IBUF(11)*SCALE)-ELANG).GT.ELTOL) THEN
                     BDBEAMS=BDBEAMS+1
C     
C     GO GRAB ANOTHER BEAM
C     
                     GOTO 1050
                  END IF
               ELSE IF (ICOPLANE.EQ.4) THEN
                  IF (ABS((IBUF(31)*SCALE+AZCOR)-ELANG).GT.ELTOL) THEN
                     BDBEAMS=BDBEAMS+1
C     
C     GO GRAB ANOTHER BEAM
C     
                     GOTO 1050
                  END IF
               END IF
               
            END IF
C     
C     NORMALIZE ELEVATION/CHECK FOR NEGATIVES
C     
C           IBUF(11)=INORM(IBUF(11))
            IBUF(11)=INANG(IBUF(11))
            ACTEL=IBUF(11)*SCALE
            NRAYS=NRAYS+1
            GDBEAMS=GDBEAMS+1
            IF (ICOPLANE.GE.1 .AND. ICOPLANE.LE.3) THEN
C
C     Coplane scan: interpolation to coplane (x,y,c) or cartesian (x,y,z)
C
               AZTMP=IBUF(10)*SCALE + AZCOR 
               IF (AZTMP.GT.360.0) AZTMP=AZTMP-360.0
               IF (AZTMP.LT.0.0)   AZTMP=AZTMP+360.0
               DIF=AZTMP - SCANGL
               SCANGL=AZTMP
               IF (ACTEL.GE.90.0 .OR. (SCANGL - BASANG).EQ.0.0) THEN
                  WRITE(*,*)'***INVALID ANGLE IN RPNCAR***'
                  STOP
               END IF
               ELSCAN(GDBEAMS)=CALCOP(ACTEL,(SCANGL - DASANG))
               IF (ELSCAN(GDBEAMS).LT.0.0 .OR. 
     X             ELSCAN(GDBEAMS).GT.90.0) THEN
                  WRITE(*,*)'***INVALID ANGLE IN RPNCAR***'
                  STOP
               END IF
               IF (ELSCAN(GDBEAMS).GT.ELMAX) ELMAX=ELSCAN(GDBEAMS)
               IF (ELSCAN(GDBEAMS).LT.ELMIN) ELMIN=ELSCAN(GDBEAMS)
               ELSUM=ELSUM+ELSCAN(GDBEAMS)
            ELSE IF (ICOPLANE.EQ.0) THEN
C
C     Normal ppi/sur scan: interpolation to cartesian (x,y,z)
C
               AZTMP=IBUF(10)*SCALE + AZCOR 
               IF (AZTMP.GT.360.0) AZTMP=AZTMP-360.0
               IF (AZTMP.LT.0.0)   AZTMP=AZTMP+360.0
               DIF=AZTMP - SCANGL
c--------------write(7,*)'Scan direction aztmp,scangl,dif=',aztmp,scangl,
c----X---------dif
               SCANGL=AZTMP
               ELSCAN(GDBEAMS)=ACTEL
               ELSUM=ELSUM+ACTEL
               IF (ACTEL.GT.ELMAX) ELMAX=ACTEL
               IF (ACTEL.LT.ELMIN) ELMIN=ACTEL
            ELSE IF (ICOPLANE.EQ.4) THEN
C
C     Rhi scan: interpolation to cartesian (x,y,z)
C
               AZTMP=IBUF(11)*SCALE
               DIF=AZTMP-SCANGL
               SCANGL=AZTMP
               ELSCAN(GDBEAMS)=IBUF(10)*SCALE + AZCOR
c               IF (IPTR.GT.129) THEN
               IF (IPTR.GT.IPTR_INT) THEN
C     
C     CHECK FOR 360 CROSSOVER. WANT FIXED ANGLES TO MONOTONICALLY INCREASE
C     
                  IF (ABS(ELSCAN(GDBEAMS)-ID(IPTR-3)/REAL(ID(44))).GT.
     X                 180.0) THEN
                     ELSCAN(GDBEAMS)=ELSCAN(GDBEAMS)+360.0
                  END IF
               END IF
               ELSUM=ELSUM+ELSCAN(GDBEAMS)
               IF (ELSCAN(GDBEAMS).GT.ELMAX) ELMAX=ELSCAN(GDBEAMS)
               IF (ELSCAN(GDBEAMS).LT.ELMIN) ELMIN=ELSCAN(GDBEAMS)
            END IF
            IF (ABS(DIF).GT.180) DIF=DIF-SIGN(360.0,DIF)
            ABDIF=ABS(DIF)
            IF (ABDIF.GT.AZMAX) AZMAX=ABDIF
            IF (ABDIF.LT.AZMIN .AND. ABDIF.GT.0.0) AZMIN=ABDIF
            DIR=DIR+DIF
c-----------write(7,*)'Scan direction dif,dir=',dif,dir
C     
C     ESTABLISH SETRANGE
C     
C     THE FOLLOWING FNCTN CALL CHECKED THAT THE # OF GATES, ETC.
C     WERE CONSTANT FOR ALL FIELDS OF THE BEAM; DOESN'T SEEM NECESS. FOR RP-7
C           LCONT=INTCHK(IBUF,RUSR2.GT.0.0)
C           IF(LCONT.GT.0) CALL CHKMSG(9,NRAYS)
C           IPTD=IBUF(5)
C           IPTF=IBUF(IPTD+4)

C     STORE # OF RANGE GATES FOR THIS BEAM IN KOUT(8)
            IF (RUSR2.EQ.0.0) THEN
               KOUT(8)=MIN(IBUF(15),MAXRNG)
               NRG=KOUT(8)
            ELSE
               KOUT(8)=ID(34)
            END IF
            IRGFG=INORM(IBUF(12))
            IADFG=INORM(IBUF(13))
            RJ1=IRGFG+IADFG*0.001
            IF (RNOTUS.NE.0.0) RJ1=RJ1+RNOTUS
            NGFH=IBUF(15)
            IF (RUSR2.GT.0.0) THEN
               JL=(RJ1-RG1)/DRG+10001.5
               JL=JL-10001
               JNG=MIN0(JL+NGFH,NGFH)
               IUNPAD=MAX0(-JL,0)
               IPUTAD=MAX0(JL,0)
               JNG=MIN0(JNG,NRG-IPUTAD)
               GOTO 220
            ENDIF
            IF(RNOTUS.EQ.0.0.AND.NINT(RJ1*1000.0).NE.IBEGRG) THEN
               PRINT 205, ID(31),ID(32),RJ1,IBEGRG
 205           FORMAT(5X,2I8,F20.10,I10)
               CALL CHKMSG(2,NRAYS)
            END IF
            IF (IBUF(14).NE.ID(33) .AND. DRGUS.EQ.0.0) THEN
C
C     RANGE GATE SPACING HAS CHANGED
C
               IDRGCHG=IDRGCHG+1
               NRAYS=NRAYS-1
               GDBEAMS=GDBEAMS-1
               BDBEAMS=BDBEAMS+1
               GOTO 1050
            END IF
            IF (IBUF(15).NE.ID(34) .AND. (DRGUS.EQ.0.0 .AND.
     X           RUSR2.EQ.0.0)) THEN
c--------------------------------------
C     NUMBER OF RANGE GATES HAS CHANGED
C        No longer considered as fatal
c               INRNG=INRNG+1
c               NRAYS=NRAYS-1
c               GDBEAMS=GDBEAMS-1
c               BDBEAMS=BDBEAMS+1
c               GOTO 1050
c--------------------------------------
            END IF
            IUNPAD=0
            IPUTAD=0
            JNG=NRG
 220        CONTINUE
            IF(ISWMTH.EQ.0) THEN
C     
C     CHECK IF FIXED ANGLE HAS CHANGED (SWEEP NUMBER MODE ONLY)
C     
               FXANG=IBUF(31)*SCALE
               IF (ELSAV.GT.360.0) FXANG=FXANG+360.0
               IF(FXANG.NE.ELSAV.AND.(.NOT.IELCHK)) 
     X              CALL CHKMSG(8,NRAYS)
            END IF
            J1=1+IPUTAD
            J2=J1+JNG-1
C     
C     CHECK AND PLUCK FIELDS NEEDED
C     
            N=1
            IFC=0
            DO I=1,MAXFLD
               MTFIEL(I)=0
            END DO
            KST=ID(37)
c            IDPTR=76
            IDPTR=IDPTR_INT
C           IUFPTR=IPTD+3
            IF(MFBM.EQ.MFTOL) THEN
C     
C     CANNOT LOCATE A REQUESTED FIELD  --LIKELY INCORRECTLY SPECIFED BY USER
C     
               PRINT 225, (I,IFIELD(I),I=1,NFLINP)
 225           FORMAT(//5X,' FIELDS REQUESTED...'/8X,
     X              ' #   SPRINT NAME'/(8X,I2,3X,A8) )
               PRINT 227
 227           FORMAT(//5X,' FIELDS PRESENT...'/8X,
     X              ' #   SPRINT NAME   ','PREFIX   EDIT CODE')
            END IF
C     
C     LOCATE ANY FIELDS USED FOR THRESHOLDING AND STORE THEM IN ARRAYS
C     
            INUM=1
            IDPTRSV=IDPTR
 230        IF (N.LE.6) THEN
               IFLDPTR=69+(N-1)
            ELSE
               IFLDPTR=161+(N-7)
            END IF
            CALL GETNAME(IBUF(IFLDPTR),NAMFLD)
            WRITE(NFLDTMP,402)NAMFLD
            WRITE(CFIELD,235)NAMFLD
 235        FORMAT(A8)
            ITIFLG=1
            DO I=1,NTHRSH
               IF (CFIELD.EQ.TFIELD(2,I)) THEN
C     
C     GO AND CONVERT THE FIELD FORMAT SCALED DATA INTO METEOROLOGICAL
C     UNITS
C     
                  CALL FFSCL(IBUF,NAMFLD,IUNPAD,THVAL,KOUT,INUM,J1,J2,
     X                 ID,IDPTR,IFC,MTFIEL,IOVER,ITIFLG,N,I,
     X                 VNY,KST,NGFLD,CTDBM,CTDBMXH,CTDBMXV,NDBMXH)
C     
C     STORE THE NAMES OF THE FIELDS ON WHICH WE WILL THRESHOLD
C     
                  WRITE(THON(INUM),305)NAMFLD
 305              FORMAT(A8)
                  INUM=INUM+1
                  IF (INUM.GT.3) THEN
                     WRITE(*,*)'***TOO MANY THRESHOLD FIELDS IN RPNCAR'
                     STOP
                  END IF
                  GOTO 340
               END IF
            END DO
 340        N=N+1
            IF (N.LE.IBUF(68)) THEN
               GOTO 230
            ELSE
               N=1
            END IF
 400        CONTINUE
            IF (N.LE.6) THEN
               IFLDPTR=69+(N-1)
            ELSE
               IFLDPTR=161+(N-7)
            END IF
            CALL GETNAME(IBUF(IFLDPTR),NAMFLD)
            WRITE(NFLDTMP,402)NAMFLD
 402        FORMAT(A8)
            IF(MFBM.EQ.MFTOL) THEN
C     
C     SUSPECTED MISSING FIELD NAMES
C     
C               IEDPRT=SHIFTL(IEDFLD,48)
               PRINT 403, N,NAMFLD,NAMPRE,IEDPRT
 403           FORMAT(8X,I2,3X,A8,6X,A2,7X,A2)
            END IF
C
C     Fill KOUT with field values and write it to disk in 
C     WRRYDK.
C
            ITIFLG=0
            DO 500 I=1,NFLINP
               WRITE(IFLDTMP,409)IFIELD(I)
 409           FORMAT(A8)
               IF (NFLDTMP.EQ.IFLDTMP) THEN
                  IF (FNUM.EQ.0 .OR. (FNUM.GT.0 .AND. RFNAM(1:8).NE.
     X                 NFLDTMP)) THEN
                     CALL FFSCL(IBUF,NAMFLD,IUNPAD,THVAL,KOUT,INUM,J1,
     X                    J2,ID,IDPTR,IFC,MTFIEL,IOVER,
     X                    ITIFLG,N,I,VNY,KST,NGFLD,CTDBM,CTDBMXH,
     X                    CTDBMXV,NDBMXH)
                  ELSE IF (FNUM.GT.0 .AND. RFNAM(1:8).EQ.NFLDTMP) THEN
C     
C     GO AND SUBSTITUTE AN ANALYTICAL FUNCTION FOR THIS FIELD
C     
                     READ (NAMFLD,411)ID(IDPTR),ID(IDPTR+1)
 411                 FORMAT (2A4)
                     IFC=IFC+1
                     MTFIEL(IFC)=I
                     ITYP=ITPFLDC(NAMFLD)
                     SCL=IBUF(79+2*(N-1))
                     IBIAS=IBUF(80+2*(N-1))
                     IF(IBIAS.GT.32767)IBIAS=IBIAS-65536
                     BIAS=IBIAS*.01
                     FACT=1.0
                     ID(IDPTR+2)=0
                     IF (ITYP.EQ.1) THEN
C     Received power field
                        IF (CFAC1.EQ.0.0) THEN
                           ID(IDPTR+2)=-1
                        ELSE IF (CFAC1.EQ.-32767.) THEN
                           WRITE(*,*)' '
                          WRITE(*,*)'+++NEED CALIBRATION INFO FOR DM ',
     X                          'FIELD+++'
                           WRITE(*,*)' '
                           STOP
                        ELSE
                           ID(IDPTR+2)=CFAC1*100.
                        ENDIF
                     ELSE IF (ITYP.EQ.3) THEN
C     Velocity field
                        IF (VNYQ.EQ.0.0) THEN
                           ID(IDPTR+2)=IBUF(21)*100.0*IBUF(20)*.0000025
                        ELSE
                           ID(IDPTR+2)=VNYQ*100.
                        ENDIF
C                       IF(IBUF(IFLDPTR+20).EQ.MFL) THEN
C                          ID(IDPTR+2)= -ID(IDPTR+2)
C                       END IF
                     ENDIF
                     IPF=101+(N-1)*IBUF(15)
                     NGFLD=IBUF(15)
                     K=IUNPAD
C     
C     SET SCALE FACTOR FOR ANALYTICAL FIELD
C     AND REPLACE MEASURED FIELD WITH ANALYTICAL FUNCTION
C     
                     IF (FNUM.EQ.9 .OR. FNUM.EQ.10) THEN
                        ID(IDPTR+4)=64
                     ELSE
                        ID(IDPTR+4)=100
                     END IF
                     CALL ANLREP(FNUM,P1,P2,P3,P4,P10,NRG,KST,J1,
     X                    J2,NGFLD,ID(IDPTR+4),K,IBUF(11)*SCALE,
     X                    IBUF(31)*SCALE,AZZ,ICOPLANE,RG1,DRG,XORR,
     X                    YORR,ZORR)
                  ELSE
                     WRITE(*,*)'***INVALID STATE FOR REPLAC. IN UFNCAR'
                     STOP
                  END IF
                  IDPTR=IDPTR+5
                  write(7,1770)gdbeams,azz,nrg,i,kst
 1770             format(1x,'UFbeam: gdbeams,azz,nrg,fld#,kst=',
     +                 i8,f8.3,i6,i4,i8)
                  KST=KST+NRG
               ENDIF
 500        CONTINUE
C           IUFPTR=IUFPTR+2
            N=N+1
            IF (N.LE.IBUF(68)) GOTO 400
            IF(IFC.NE.NFLINP) THEN
C     
C     MISSING FIELD - TOSS BEAM
C     
               MFBM=MFBM+1
               IF (MFBM.GT.MFTOL) CALL CHKMSG(7,NRAYS)
c--------------PRINT 515,NSWPS,GDBEAMS
 515           FORMAT (5X,'*** MISSING FIELD IN SCAN ',I2,'  BEAM ',I4,
     X              ' - BEAM DISCARDED,STATISTICS FOR THIS SCAN ',
     X              'MAY BE AFFECTED')
               NRAYS=NRAYS-1
               GDBEAMS=GDBEAMS-1
               BDBEAMS=BDBEAMS+1
C     
C     GO GET ANOTHER RAY
C     
               GOTO 1050
            END IF
C     
C     AT THIS POINT ALL FIELDS HAVE BEEN READ FROM TAPE INTO ARRAYS.
C     NOW WE DO THE ACTUAL THRESHOLDING.
C     
            
            IDPTRN=IDPTRSV
            KSTN=ID(37)
C     
C     LOOP OVER THE FIELDS READ FROM DISK FILE
C     
            DO 600 K=1,NFLINP
               WRITE(CTEMP1,521)ID(IDPTRN),ID(IDPTRN+1)
 521           FORMAT(2A4)
               DO 560 I=1,NTHRSH
                  IF (TFIELD(1,I).EQ.CTEMP1) THEN
                     CTEMP2=TFIELD(2,I)
                     IF(CTEMP2.EQ.THON(1)) THEN
                        IKEY=1
                     ELSE IF (CTEMP2.EQ.THON(2)) THEN
                        IKEY=2
                     ELSE
                        WRITE(*,*)'***INVALID STATE IN RPNCAR***'
                     END IF
C     
C     GET UPPER AND LOWER LIMITS OF FIELD ON WHICH WE'RE THRESHOLDING
C     
                     TLLIMIT=TLIMITS(1,I)
                     TULIMIT=TLIMITS(2,I)
                     DO 540 J=1,NRG
                        IF (J.LT.J1 .OR. J.GT.J2) GOTO 540
                        IF (J.GT.NGFLD) GOTO 540
                        IF ((THVAL(IKEY,J).LT.TLLIMIT .OR. 
     X                       THVAL(IKEY,J).GT.TULIMIT) .AND. ISIDE(I)
     X                       .EQ.1) THEN
                           KOUT(KSTN+J)=IBAD
                           ITHR(I)=ITHR(I)+1
                        ELSE IF ((THVAL(IKEY,J).GE.TLLIMIT .AND.
     X                          THVAL(IKEY,J).LE.TULIMIT) .AND.
     X                          ISIDE(I).EQ.2) THEN
                           KOUT(KSTN+J)=IBAD
                           ITHR(I)=ITHR(I)+1
                        END IF
C     
C     THIS NEXT ARRAY ELEMENT WILL CONTAIN THE TOTAL NUMBER OF POINTS IN VOL. SCAN
C     
                        IF (I.EQ.1) ITHR(MAXFLD+1)=ITHR(MAXFLD+1)+1
 540                 CONTINUE
                  END IF
 560           CONTINUE
               KSTN=KSTN+NRG
               IDPTRN=IDPTRN+5
 600        CONTINUE
            
            MFBM=0
C     
C     CHANGE INDEXING OF FIELDS
C     
            DO I=1,NFLINP
               NTM(I)=IFIELD(I)
               ITM(I,1)=INTINF(I,1)
               ITM(I,2)=INTINF(I,2)
               ITM(I,3)=INTINF(I,3)
            END DO
            DO I=1,NFLINP
               INDEX=MTFIEL(I)
               IFIELD(I)=NTM(INDEX)
               INTINF(I,1)=ITM(INDEX,1)
               INTINF(I,2)=ITM(INDEX,2)
               INTINF(I,3)=ITM(INDEX,3)
            END DO
            CALL RDRAY(NWDS,NST)
            IF (NST.GT.0) THEN
C     
C     READ ERROR
C     
               IALREAD=1
C     
C     GO PROCESS ERROR CONDITION
C     
               GOTO 1050
            END IF
            NLEN=NFLINP*NRG+ID(37)
            IFLG=0
            CALL WRRYDK(KPCK,KOUT,NST,LTMP,IFLG,NLEN)
C     
C     *** BAD READ ***
C     
         ELSE IF (NST.NE.0) THEN
            IF (NST.EQ.1) IEOF=IEOF+1
C     
C     LIMIT NUMBER OF CONSECUTIVE END OF FILES TO 9
C
            IF (IEOF.GE.10) THEN
               WRITE(*,*)'***MORE THAN 9 EOFs--WRAPPING UP VOLUME***'
               IEOF=0
               GOTO 1060
            END IF
C     
C     IROV= (-1) APPEND, (0) NORMAL PROCESSING, (1) RUNOVER
C     
            IF (IROV.EQ.0) THEN
               GOTO 1060
            ELSE IF (IROV.GT.0 .AND. NST.EQ.3) THEN
C     
C     RUNSOVER ONTO NEXT TAPE
C     
               IROV=1-IABS(IROV)
               READ 613,KRD
 613           FORMAT (10A8)
               READ (KRD,615)KOMM
 615           FORMAT (A3)
               IF (KOMM.NE.'INP') THEN
                  PRINT 617
 617              FORMAT (5X,'***ERROR - ',
     X                 'INPUT CARD MUST FOLLOW RUNOVER PROCESS CARD')
                  STOP 617
               ENDIF
               IJNK=1
               CALL INPFIL(KRD,NSTP,IJNK,AZCOR,USER_DEF_ORIGIN,
     X                     RADAR_CHOSEN,IROV)
               IF (ISKP.GT.0) THEN
C     
C     SKIP VOLUMES ON INPUT UNIT BEFORE PROCESSING
C     
                  CALL SKPVOL(IUN,ISKP)
                  ISKP=0
               END IF
               
            ELSE IF (IROV.EQ.1) THEN
C     
C     HANDLE RUNOVER CASE
C     
               CALL NWRAY(NWDS,MBYTE,NST,SWAPPING)
               MRAYS=MRAYS+1
               IPREC=IPREC+1
               NAZZ=NAZZ+1
               IF(IFD.EQ.1)THEN
                  IF(MOD(MRAYS,IFD_RAYS).EQ.0)THEN
                     CALL FFDUMP(IBUF,NWDS,MAXIN,NST,SCALE,IPREC,NAZZ)
                  END IF
               END IF
               IF (NST.EQ.3) THEN
                  IALREAD=1
                  GOTO 1050
               ELSE
                  GOTO 1060
               END IF
            END IF
C     
C     CHECKS ON NEW VOLUME
C     
            NST=0
            OLDAZ=SCANGL
            OLDEL=ELSAV
            LASTIM=ID(7)*10000 + ID(8)*100 + ID(9)
            IDIR=1
            IF (DIR.LT.0) IDIR=-1
            IKAZ=0
            IKNT=0

            DO 1045 ILP1=1,1000000
C     
C     BODY OF REPEAT LOOP
C     
               IF(NST.EQ.3) GO TO 1060
               CALL NWRAY(NWDS,MBYTE,NST,SWAPPING)
               MRAYS=MRAYS+1
               IPREC=IPREC+1
               NAZZ=NAZZ+1
               IF(IFD.EQ.1)THEN
                  IF(MOD(MRAYS,IFD_RAYS).EQ.0)THEN
                     CALL FFDUMP(IBUF,NWDS,MAXIN,NST,SCALE,IPREC,NAZZ)
                  END IF
               END IF
               IF (NST.EQ.3 .AND. IROV.EQ.-1) THEN
                  write(7,*)'UFNCAR: end-of-data for this file'
                  GOTO 1060
               END IF
               IF (NST.EQ.3 .AND. IROV.GT.0) THEN
                  IALREAD=1
                  GOTO 1050
               END IF
               IF (NST.GT.0) THEN
C     
C     DUMB CONSTANT FOR TRANSFER BACK UP LOOP
C     
                  IJ = 1
                  GOTO 120
               ELSE
                  IJ = 0
               END IF
               IF (IKAZ.LE.0) THEN
                  IF (IKNT.EQ.0) THEN
                     LFTIM=0
C     MARK BRADFORD PATCH TO REMOVE CONVERSION TO MDT
C     IF (IFUT.EQ.'UT') LFTIM=-6
                  ENDIF
                  IHR=IBUF(7)+LFTIM
                  IF (IHR.LT.0) IHR=IHR+24
                  IFTIM=10000*IHR + 100*IBUF(8) + IBUF(9) 
                  if(ifd.eq.1)then
                     write(7,*)'RPNCAR: iftim,ketim=',iftim,ketim
                  end if
                 IF(IFTIM.GT.KETIM) THEN
C     
C     ENDING PROCESS TIME SURPASSED WHILE ATTEMPTING AN APPEND.
C     WRAP UP CURRENT SCAN AND TERMINATE THE PROCESSING OF THE VOLUME.
C     
                     BACKSPACE IUN
                     IROV=0
                     NST=1
                     GO TO 1060
                  END IF
                  IF (IKNT.EQ.0) PRINT 711,IFTIM
 711              FORMAT (5X,'     APPEND VOLUME AT TIME ',I6)
                  IKNT=IKNT+1
                  IF (IFTIM.LT.LASTIM) GOTO 1045
C     
C     NORMALIZE ELEVATION and CHECK FOR NEGATIVES
C     
C                 IBUF(31)=INORM(IBUF(31))
                  IF (ICOPLANE.NE.4) IBUF(31)=INANG(IBUF(31))
                  CUREL=IBUF(31)*SCALE
                  IF (ABS(CUREL-OLDEL).GT.FXSTOL) THEN
                     IF (NST.NE.0) THEN
                        WRITE(*,*)'***BAD STATE IN RPNCAR***'
                        STOP
                     END IF
                     IALREAD=1
                     GOTO 1050
                  END IF
               END IF

               IKAZ=IKAZ+1
C              ISWP=IBUF(10)
               IF (ICOPLANE.LT.4) THEN
                  CURAZ=IBUF(10)*SCALE + AZCOR
                  IF (CURAZ.GT.360.0) CURAZ=CURAZ-360.0
                  IF (CURAZ.LT.0.0) CURAZ=CURAZ+360.0
               ELSE IF (ICOPLANE.EQ.4) THEN
                  CURAZ=IBUF(11)*SCALE
               END IF
               IF (ABS(((CURAZ-OLDAZ)*IDIR)).GT.0.0) THEN
                  IF (NST.NE.0) THEN
                     WRITE(*,*)'***BAD STATE IN RPNCAR***'
                     STOP
                  END IF
                  IALREAD=1
                  GOTO 1050
               END IF
C     
C     UNTIL CONDITION
C     
 1045       CONTINUE
         END IF
C----------------------------------------+
C     Bottom of main loop over beams     | 
C----------------------------------------+
 1050 CONTINUE
      
 1060 CONTINUE
C     
C     WRAP UP THIS VOLUME AND ELEVATION SCAN
C     
      write(7,*)'FF (eos): ivol,iswp,gdbeams=',ivol,iswp,gdbeams
      IF (GDBEAMS.LT.MNBEM .AND. IFXTAB.NE.1) THEN
C         CALL WRRYDK(KPCK,KOUT,INST,LTMP,-9,0)
         IF (GDBEAMS.GT.0) THEN
            IFLG=NSWPS+11
            CALL WRRYDK(KPCK,KOUT,INST,LTMP,IFLG,GDBEAMS)
c-----------debug (ljm)
            write(7,*)'FFvol (eov): nswps,gdbeams=',nswps,gdbeams
c-----------debug (ljm)
            WRITE(*,123)NSWPS+1,GDBEAMS
         END IF
         NRAYS=NRAYS-GDBEAMS
      ELSE
         ELFIX=ELSAV
C     
C     RE-COMPUTE FIXED ANGLE IF REQUESTED
C     
         IF(IELCHK) THEN
            I1=IGNEL+1
            I2=GDBEAMS-IGNEL
            IF(I2.LT.I1) THEN
               I1=GDBEAMS/2+1
               I2=I1
            END IF
            SUM=0.0
            DO 1080 I=I1,I2
               SUM=SUM+ELSCAN(I)
               J=NINT(ELSCAN(I)*10.0)+1
               IF(J.LT.1.OR.J.GT.MXL) GO TO 1080
               ELSCAN(MXSCN+J)=ELSCAN(MXSCN+J)+1.0
 1080       CONTINUE
            IF(IELT.EQ.1) THEN
               MODE=0
               ELMOD=0.0
               DO 1090 I=1,MXL
                  IF(ELSCAN(MXSCN+I).LE.ELMOD) GO TO 1090
                  MODE=I
                  ELMOD=ELSCAN(MXSCN+I)
 1090          CONTINUE
               IF(MODE.NE.0) ELFIX=(MODE-1)*0.1
            ELSE IF (IELT.EQ.2) THEN
               ELFIX=SUM/(I2-I1+1)
            END IF
         END IF
         ID(IPTR)=NINT(ELFIX*ID(44))
         IF (ICOPLANE.EQ.4 .AND. .NOT.IELCHK .AND. 
     X        AZCOR.NE.0.0) THEN
C
C     ADJUST FIXED ANGLE BY AZIMUTH CORRECTION TOO FOR RHIs
C
            ELFIX=ELFIX+AZCOR
            ID(IPTR)=NINT(ELFIX*ID(44))
         END IF
         ID(IPTR+1)=SIGN(1.0,DIR)
C
C     REVERSE DIRECTION OF SCAN FOR RHI SCANS
C
         ELTMP=ELFIX
         IF (ELTMP.GT.360.) ELTMP=ELTMP-360.
         IF (ICOPLANE.EQ.4 .AND. ((ELTMP.LT.45.0 .AND.
     X        ELTMP.GE.0) .OR. (ELTMP.LT.315. .AND.
     X        ELTMP.GE.225.0) .OR. (ELTMP.LT.405 .AND.
     X        ELTMP.GE.315))) ID(IPTR+1)=-ID(IPTR+1)
         ID(IPTR+2)=NRAYS
         NSWPS=NSWPS+1
         IF (NSWPS.GT.MAXEL) THEN
            PRINT 1097,MAXEL
 1097       FORMAT(/,5X,'+++ MAX. NUMBER OF SWEEPS IS ',I3,' +++')
            STOP 1097
         ENDIF
         IF (ISKIPFX.EQ.0) THEN
            IF (GDBEAMS.GE.2) THEN
               DELAZ=ABS(DIR)/(GDBEAMS-1)
               DELEL=ELSUM/GDBEAMS
            ELSE
               DELAZ=0.0
               DELEL=0.0
            END IF
         ELSE
            DELAZ=0.0
            DELEL=0.0
         END IF
         IF(ICOPLANE.EQ.4)THEN
            FXSAV=ELSAV+AZCOR
         ELSE
            FXSAV=ELSAV
         END IF
         IF(IFD.EQ.1)THEN
            WRITE(7,1105)NSWPS,ID(IPTR+1),FXSAV,ELMIN,ELMAX,DELEL,
     X           BEGAZ,SCANGL,AZMIN,AZMAX,DELAZ,GDBEAMS,BDBEAMS,VNYQSWP
         END IF
         PRINT 1105,NSWPS,ID(IPTR+1),FXSAV,ELMIN,ELMAX,DELEL,
     X        BEGAZ,SCANGL,AZMIN,AZMAX,DELAZ,GDBEAMS,BDBEAMS,VNYQSWP
 1105    FORMAT(2(4X,I2),8X,4(F6.2,3X),4X,2(F6.1,4X),
     X        3(F6.2,3X),I7,I6,F7.2)
         VNYQUIST(NSWPS)=VNYQSWP
         IPTR=IPTR+3
      ENDIF
C     
C     DO END OF VOLUME PROCESSING
C     
 2000 CONTINUE
C     
C     VOLUME IS FINISHED
C     
      IFLG=9
      CALL WRRYDK(KPCK,KOUT,INST,LTMP,IFLG,0)
c-----debug (ljm)
      write(7,*)'FFvol: End-of-volume'
c-----debug (ljm) 
      IF (NSWPS.LT.2 .AND. IPPI.EQ.0) THEN
         write(7,*)'RPNCAR: irov=',irov
         IF(IROV.EQ.0)PRINT 2013,NSWPS
 2013    FORMAT(5X,'*** VOLUME DISCARDED - ONLY ',I1,
     X        ' ELEVATION SCANS')
C     
C     GO FIND A NEW VOLUME
C     
         GOTO 200
      ENDIF
c      J=129
      J=IPTR_INT
      NSWPM1 = NSWPS - 1
      ILSCHK=.FALSE.
      LAST=ID(J)
      DIFFE=(IABS(ID(J)-ID(J+3)))
      DO 2020 I=1,NSWPM1
         K=J+3
         IF (ICOPLANE.EQ.1) THEN
            DIFFH=(IABS(ID(K)-LAST))
            IF (ABS(DIFFH-DIFFE).GT.(.2*ID(44))) THEN
               WRITE(*,2015)
 2015          FORMAT(5X,'***ERROR: COPLANE ANGLES OF DATA ARE NOT',
     X              'UNIFORMLY SPACED. REINTERPOLATE AND SPECIFY',
     X              'TARGET ANGLES EXPLICITLY WITH GRIDCPL CARD. ')
               STOP 2015
            END IF
         END IF
         IF(ID(K).LE.LAST) THEN
            ILSCHK=.TRUE.
            GO TO 2021
         END IF
         LAST=ID(K)
         J=K
 2020 CONTINUE
 2021 CONTINUE
      ID(36)=NRAYS
      ID(35)=NSWPS
      REWIND LTMP
      IELCHK=IELCHK.OR.ILSCHK
      print *,'FF: ielchk,ilschk=',ielchk,ilschk
C     
C     FORCE GENERATION OF SECOND TABLE OF SCAN STATS IF USER IS USING FXTABLE 
C     
      IF (IFXTAB.EQ.1 .AND. (ISKIP.EQ.1 .OR. IACCPT.EQ.1))IELCHK=.TRUE.
      CALL GENAZM(JPCK,IELCHK,MNBEM,ELTOL,NAST,ICOPLANE,BASANG)
      IF (NAST.NE.0) THEN
         PRINT 2037
 2037    FORMAT(5X,'*** VOLUME DISCARDED - INPUT DATA STRUCTURE ',
     X        'CANNOT BE INTERPOLATED.')
C     
C     GO FIND A NEW VOLUME
C     
         GOTO 200
      ENDIF
      CALL SETVOL
      NUFST=0
C     
C     PRINT OUT NUMBER OF DATA POINTS SET TO BAD DUE TO THRESHOLDING
C     
      WRITE(*,*)' '
      IF (NTHRSH.GT.0) THEN
         WRITE(*,2041)
 2041    FORMAT(15X,'POINTS SET TO BAD DUE TO INSIDE/OUTSIDE TESTS')
         WRITE(*,2044)
 2044    FORMAT(15X,'------ --- -- --- --- -- -------------- -----')
         WRITE(*,2047)
 2047     FORMAT(/,10X,'FIELD        % SET TO BAD  # SET TO BAD  ',
     X        'TOTAL # OF POINTS')
         WRITE(*,2050)
 2050    FORMAT(10X,'---------    ------------  ------------  ',
     X        '-----------------')
      END IF
      DO I=1,NTHRSH
         THRPER=(FLOAT(ITHR(I))/FLOAT(ITHR(MAXFLD+1)))*100
         WRITE(*,2051)TFIELD(1,I),THRPER,ITHR(I),ITHR(MAXFLD+1)
 2051    FORMAT(10x,A8,7x,F8.1,7x,I8,6x,I8)
         ITHR(I)=0
      END DO
      WRITE(*,*)' '
C     
C     PRINT OUT NUMBER OF OVERFLOWED FIELD VALUES, IF ANY
C     
      IF (IOVER(1).NE.0) WRITE(*,2052)IOVER(1)
 2052 FORMAT(//I4, ' VALUES OVERFLOWED IN POWER FIELD IN THIS VOLUME')
      IF (IOVER(2).NE.0) WRITE(*,2053)IOVER(2)
 2053 FORMAT(/I4,' VALUES OVERFLOWED IN REFLECTIVITY FIELD IN THIS ',
     X     'VOLUME')
      IF (IOVER(3).NE.0) WRITE(*,2054)IOVER(3)
 2054 FORMAT(/I4, ' VALUES OVERFLOWED IN VELOCITY FIELD IN THIS ',
     X     'VOLUME')
      IF (IOVER(4).NE.0) WRITE(*,2055)IOVER(4)
 2055 FORMAT(/I4, ' VALUES OVERFLOWED IN VARIANCE FIELD IN THIS ',
     X     'VOLUME')
      IF (IOVER(5).NE.0) WRITE(*,2056)IOVER(5)
 2056 FORMAT(/I4, ' VALUES OVERFLOWED IN CORRELATION FIELD IN THIS ',
     X     'VOLUME')
      IF (IOVER(6).NE.0) WRITE(*,2057)IOVER(6)
 2057 FORMAT(/I4, ' VALUES OVERFLOWED IN UNKNOWN FIELD IN THIS ',
     X     'VOLUME')
      DO I=1,6
         IOVER(I)=0
      END DO
C     
C     WRITE OUT THE NUMBER OF BEAMS DISCARDED DUE TO A CHANGE IN THE GATE
C     SPACING AND THE NUMBER DISCARDED DUE TO A CHANGE IN THE # OF GATES
C     
      IF (IDRGCHG.GT.0) THEN
         WRITE(*,2059)IDRGCHG
 2059    FORMAT(/,I5,' BEAMS DISCARDED DUE TO A CHANGE IN THE GATE ',
     X        'SPACING',/)
      END IF
      IF (INRNG.GT.0) THEN
         WRITE(*,2060)INRNG
 2060    FORMAT(/,I5,' BEAMS DISCARDED DUE TO A CHANGE IN THE # OF ',
     X        'RANGE GATES',/)
      END IF
      
      IF (ICOPLANE.EQ.4 .AND. ANGXAXT.NE.90.0) THEN
         
C     RESET AZIMUTH CORRECTION BACK TO ORIGINAL VALUE
         
         AZCOR=AZCORT
         ANGXAX=ANGXAXT
      END IF

      RETURN
      END
      
