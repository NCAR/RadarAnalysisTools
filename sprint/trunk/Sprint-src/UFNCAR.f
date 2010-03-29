      SUBROUTINE UFNCAR(NUFST,JPCK,ELSCAN,ICRTST,NTHRSH,
     X     TFIELD,TLIMITS,RFNAM,FNUM,P1,P2,P3,P4,P10,AZCOR,DASANG,
     X     ISIDE,USER_DEF_ORIGIN,RADAR_CHOSEN,VNYQUIST,IFD,IFD_RAYS)
C     
C     THIS SUBROUTINE READS ONE VOLUME SCAN IN UNIVERSAL FORMAT
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
C     ISWMTH: use (0) sweep number or (1) fixed angle to delimit sweeps (see PROFIL)
C     IROV:   (-1) append used to merge consecutive scan volumes in the same file
C             ( 0) normal processing - scan volumes and files coincide
C             ( 1) runover used to merge scan volume split across files
C     IFD:    (0) do not, (1) do dump beam header every (IFD_RAYS)th beam
C
C     Universal format significant housekeeping words:
C         7 - Volume scan number
C         8 - Ray number within a volume scan
C        10 - Sweep number within this volume scan
C        26 - Year of the data (last two digits)
C        27 - Month of the data
C        28 - Day of the data
C        29 - Hour of the data
C        30 - Minute of the data
C        31 - Second of the data
C             CF = 64 = JRH6
C        33 - Current azimuth angle (degrees * CF)
C        34 - Current elevation angle (degrees * CF)
C        35 - Scan mode: (0) Calibration, (1) PPI or sector, (2) Coplane,
C                        (3) RHI, (4) Vertical, (5) Target, (6) Manual, 
C                        (7) Idle, (8) Surveillance.
C        36 - Fixed angle (degrees * CF)
C        45 - Missing data flag (usually 100000 octal or -32768)
C     UF optional header:
C         5 - Primary coplane baseline angle (degrees * CF)
C        14 - (0) ranging information same for all fields, 
C             (1) same within sweep, (2) same within each ray
C     UF data header:
C         1 - Number of fields this ray
C     UF field header:
C         3 - range to first gate
C         5 - range gate spacing
C         6 - number of range gates
C        12 - Primary wavelength (cm * 100)
C        20 - Nyquist velocity or radar constant
C        21 - FL (2 ASCII) if flagged in least significant bit
C             with NCAR bad velocity flag (1 - good, 0 - bad)
C
C     ANGXAX = ANGLE OF X AXIS FROM TRUE NORTH (USUALLY 90.0)
C              Note: Use ANGXAXT to store user-specified ANGXAX
C     BASANG = ANGLE OF Y AXIS FROM TRUE NORTH; USED IN COPLANE SCANS
C     DASANG = ANGLE OF Y AXIS FROM BASELINE OF RADARS; USED IN COPLANE SCANS
C        Values of BASANG and DASANG:
C           - Not used for non-coplane scans
C           - Same value when interpolating from coplane scans to coplane
C           - May be different when interpolating from coplane scans to xyz
C     
C     GDBEAMS - Counter for acceptable beams incremented here.
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
c      PARAMETER (MAXSKP=27)

      DIMENSION MTFIEL(MAXFLD)
      DIMENSION JPCK(1),ELSCAN(1),ISIDE(MAXFLD)
      DIMENSION TLIMITS(2,MAXFLD),THVAL(2,MAXRNG),ITHR(MAXFLD+1)
      DIMENSION VNYQUIST(MAXEL)

      CHARACTER*8 KRD(10),RFNAM,P10,NTM(MAXFLD)
      CHARACTER*8 CTEMP1,CTEMP3,NAMFLD
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
      CHARACTER*3 KOMM
      CHARACTER*8 INP,IOUT,MET,ICRT,IEMPTY,NAMTIM
      CHARACTER*2 MUF,MFL

      INTEGER CVMGP
      INTEGER FNUM
      INTEGER GDBEAMS,BDBEAMS

      DATA INP,IOUT,MET,ICRT/'INPUT','OUTPUT','INTERP','GRID'/
      DATA IEMPTY,MUF,MFL/'-9999','UF','FL'/
      DATA MFTOL/25/
      DATA IEDBD1/ O'777' /

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
C        INANG: Change scaled (x 64) elevations in 350-360 range negative (0 TO -10)
C        CALCOP: Calculates coplane angle from elevation and azimuth angle
C        CALEL: Calculates elevation angle from fixed and azimuth angle.
C        CALHAZ: Calculate horizontal azimuth from azimuth in coplane and coplane angle.
C
      INORM(I)=CVMGP(I-65536,I,I-32768)
      INANG(I)=CVMGP(I-23040,I,I-22400)
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
      SCALE   = 1.0/FLOAT(JRH6)
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
      print *,'UF: ielchk,ilschk,eltol,fxstol=',
     +     ielchk,ilschk,eltol,fxstol
C
C     CHECK TO MAKE SURE USER IS NOT REQUESTING INTERP. TO DEFAULT
C     COPLANE ANGLES AND REQUESTING A REDEF. OF FIXED ANGLES
C
      IF (ICOPLANE.EQ.1 .AND. IELCHK) THEN
         WRITE(*,*)'***CANNOT INTERPOLATE TO DEFAULT COPLANE ANGLES',
     X             ' OF DATA AND REDEFINE FIXED ANGLE AS MEAN OR MODE'
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
      write(7,*)'UFint: unit,jst,iflg,len,plen=',
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
            write(7,*)'First UF record in this file'
            CALL UFDUMP(IBUF,NWDS,MAXIN,NST,SCALE,IPREC,NAZZ)
         END IF
      END IF
      IF (NST.NE.0) THEN
         PRINT 51,ITAP,IUN
 51      FORMAT (5X,'**END OF INFORMATION ON TAPE ',A6,' UNIT ',I3)
         NUFST=2
         ANGXAX = ANGXAXT
         RETURN
      END IF  
C     
C     CHECK IF VOLUME IS THE ONE REQUESTED AND IS A SCAN MODE THAT CAN BE 
C     INTERPOLATED.  Acceptable modes are: (1) PPI or sector, (2) Coplane, 
C                                          (3) RHI, and (8) Surveillance.
      IFUT=IBUF(32)
C      CALL SHILBL(IFUT,1)
      LFTIM=0

C     MARK BRADFORD PATCH TO REMOVE CONVERSION TO MDT
C     IF (IFUT.EQ.'UT') LFTIM=-6
C
      IHR=IBUF(29)+LFTIM
      IF (IHR.LT.0) IHR=IHR+24
      IFTIM=10000*IHR+100*IBUF(30)+IBUF(31)
      IF(IBUF(26).GT.99)IBUF(26)=IBUF(26)-100
      JDAY=10000*IBUF(26)+100*IBUF(27)+IBUF(28)
      IF(JDAY.GT.991231)THEN
         LDAY=1000000*(JDAY/1000000)
         JDAY=JDAY-LDAY
      END IF
      IF (KETIM.NE.999999) THEN
C     Position by date and time; else any volume is ok
         IF (JDAY.LT.KDAY) THEN
            PRINT 57, JDAY,IFTIM
 57         FORMAT(' +++  VOLUME SKIPPED  -  DAY: ',I8,
     X           5X,'BEGINNING TIME: ',I8,'  +++')
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
C
C     First try reading beams and checking beam time within 
C     a sweep before skipping whole sweeps as done in SKPVOL.
C     Commented out SKPVOL to see if can seek by beam.
C
c-----------CALL SKPVOL(IUN,1)
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
C     IBUF(35) - Scan mode: (0) Calibration, (1) PPI or sector, (2) Coplane,
C                           (3) RHI, (4) Vertical, (5) Target, (6) Manual, 
C                           (7) Idle, (8) Surveillance.
c-----debug (ljm)
      write(7,*)'UFvol: Beginning-of-volume'
      writ_once = .false.
c-----debug (ljm)

      JDAY=10000*IBUF(26)+100*IBUF(27)+IBUF(28)
      IF(JDAY.GT.991231)THEN
         LDAY=1000000*(JDAY/1000000)
         JDAY=JDAY-LDAY
      END IF
      PRINT 63,JDAY,IFTIM
 63   FORMAT (//100('+')//
     X     8X,'VOLUME FOUND   -   DAY : ',I6.6,
     X     8X,'BEGINNING TIME : ',I6)
      IF(IBUF(35).NE.1 .AND. IBUF(35).NE.2 .AND.
     X   IBUF(35).NE.3 .AND. IBUF(35).NE.8) THEN
         PRINT 65,IBUF(35)
 65      FORMAT (5X,'***SCAN MODE ',I1,' CANNOT BE INTERPOLATED ',
     X        '- VOLUME DISCARDED***')
         CALL DUMBUF(IBUF,150,'UF',1)
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
         IF (IBUF(35).NE.2) THEN
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
         IF (IBUF(35).EQ.2) THEN
C
C     Interpolation from Coplane scans to Cartesian grid
C
            ICOPLANE=3
         ELSE IF (IBUF(35).EQ.3) THEN
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
     X         'GRID+++')
      ELSE IF (ICOPLANE.EQ.3) THEN
         IF (DASANG.EQ.-999.0) DASANG=IBUF(IBUF(3)+4)*SCALE
         BASANG=DASANG
         WRITE(*,79)
 79      FORMAT(5X,'+++GRIDDING DATA FROM COPLANE TO 3-D CARTESIAN+++')
      ELSE IF (ICOPLANE.EQ.4) THEN
         WRITE(*,81)
 81      FORMAT(5X,'+++GRIDDING DATA FROM RHI TO 3-D CARTESIAN+++')
      END IF

      WRITE(CTEMP3,419)IBUF(1)
      IF (CTEMP3.NE.MUF) THEN
         PRINT 83,ITAP
 83      FORMAT(5X,'***ERROR IN FORMAT OF TAPE ',A6,'- MAY NOT BE UF')
      ENDIF

      IF (ICOPLANE.EQ.4 .AND. ANGXAX.NE.90.0) THEN
C
C     DUE TO CONTORTED WAY THAT RHIs HAVE TO BE INTERPOLATED,
C     X-AXIS ROTATIONS NEED TO BE HANDLED VIA AZIMUTH CORRECTION.
C         
         AZCORT=AZCOR
         AZCOR=AZCOR+(90.-ANGXAX)
         ANGXAX=90.
         WRITE(*,85)AZCOR,ANGXAXT
 85      FORMAT(5X,'+++RHI AZIMUTHS WILL BE ROTATED ',
     X        F6.2,' DEGRESS SINCE AZIMUTH +X-AXIS SPECIFIED AS ',
     X        F6.2,' INSTEAD OF 90 DEG+++')
         PRINT *,'     +++FOR RHI SCAN -  ROTATE IN CEDRIC+++'
      END IF
C     
C     FOUND VOLUME - START PROCESSING IT
C     
c-----debug (ljm)
      write(7,*)'UFvol: Volume found CALL INITVOL'
      writ_once = .false.
c-----debug (ljm)

      CALL INITVOL(IPROJ)
      INRNG   = 0
      IDRGCHG = 0
      MFBM    = 0
      ID(4)   = IHR
      IVOL    = IBUF(7)
      ISWP    = IBUF(10)
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
      IBUF(33)=INORM(IBUF(33))
      IBUF(36)=INORM(IBUF(36))
      IBUF(36)=INANG(IBUF(36))
      ELSAV=IBUF(36)*SCALE
C      IPTR=129
      IPTR=IPTR_INT
      print *,'UFNCAR: pointer to start of elev info=',iptr
C      IF (IPTR.GT.129 .AND. ABS(ELSAV-ID(IPTR-3)/REAL(ID(44))).GT.
      IF (IPTR.GT.IPTR_INT .AND. ABS((ELSAV+AZCOR)-ID(IPTR-3)/
     X     REAL(ID(44))).GT.180.0) THEN
         ELSAV=ELSAV+360.0
      END IF
      DIR=0.0
C
C     SCANGL - VALUE OF THE ANGLE [AZIMUTH (33) or ELEVATION (34)]
C              THAT CHANGES DURING THE SCAN (PPI or RHI).
C
      IF (ICOPLANE.LT.4) THEN
C     
C     FOR PPI and COPLANE CASES, SCANGL IS AZIMUTH ANGLE [IBUF(33)].
C     
         SCANGL=IBUF(33)*SCALE + AZCOR
         IF (SCANGL.GT.360.0) SCANGL=SCANGL-360.0
         IF (SCANGL.LT.0.0) SCANGL=SCANGL+360.0
      ELSE IF (ICOPLANE.EQ.4) THEN
C     
C     FOR RHI CASE, SCANGL IS ELEVATION ANGLE [IBUF(34)].  ROLES OF
C     ELEVATION AND AZIMUTH ANGLES WILL REVERSED.  AN "AZIMUTH" ANGLE
C     IN THE SCAN PLANE WILL BE AZC = 90 - ELEV.
C     
         SCANGL=IBUF(34)*SCALE
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
         fxang = ibuf(36)*scale
         el    = ibuf(34)*scale
         az    = ibuf(33)*scale
         write(7,*)
     +        'UF (bov): sw-fxea,maxin=',iswp,fxang,el,az,maxin
         writ_once = .true.
      end if
c-----debug (ljm)
C-------------------------------------+
C     Top of main loop over beams     | 
C-------------------------------------+
      DO 1050 ILP=1,1000000
         IF (ILP.EQ.1000000) THEN
            WRITE(*,*)'ERROR-TOO MANY RAYS IN UFNCAR'
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
                  CALL UFDUMP(IBUF,NWDS,MAXIN,NST,SCALE,IPREC,NAZZ)
               END IF
            END IF
            IBUF(33)=INORM(IBUF(33))
         END IF
C
C     HERE WE BRANCH BASED ON THE STATUS OF THE READ
C     
 120     CONTINUE
         IF (NST.EQ.0 .OR. IJ.EQ.1) THEN
            IF (IJ.EQ.1) IJ=0
            IF (NST.EQ.0) IEOF=0
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
               IF (ISWMTH.NE.0) THEN
                  ELCUR=ELSAV
                  IF (ELCUR.GT.360.0) ELCUR=ELCUR-360.0
                  IF (ABS(IBUF(36)*SCALE-ELCUR).LT.FXSTOL) GOTO 1050
               ELSE 
                  IF (IBUF(10).EQ.ISWP) GOTO 1050
               END IF
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
               AZZ=IBUF(33)*SCALE + AZCOR
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
               ID(51)=BASANG*FLOAT(JRH6)
               ELL=IBUF(34)*SCALE
               COP=IBUF(36)*SCALE
               IF (AZZ.EQ.90.0) THEN
                  AZC=90.0
               ELSE IF (COP.LT.90.0) THEN
                  AZC=(ATAN2(TAN(AZZ*DTR),COS(COP*DTR))/DTR)
                  IF (AZC.LT.0) AZC=AZC+180.0
               ELSE
                  WRITE(*,*)'***BAD ANGLES IN UFNCAR***'
                  STOP
               END IF
               KOUT(1)=NINT(AZC*FLOAT(JRH6))

            ELSE IF (ICOPLANE.EQ.0 .OR. ICOPLANE.EQ.3) THEN
C     
C     Constant elevation or coplane scan: interpolation to cartesian (x,y,z)
C     
               AZZ=IBUF(33)*SCALE + AZCOR - (ANGXAX-90.0)
               IF (AZZ.GT.360.0) AZZ=AZZ-360.0
               IF (AZZ.LT.0.0) AZZ=AZZ+360.0
               AZTMP=(IBUF(33)*SCALE + AZCOR)
               IF (AZTMP.GT.360.0) AZTMP=AZTMP-360.0
               IF (AZTMP.LT.0.0)   AZTMP=AZTMP+360.0
               KOUT(1)=NINT((AZTMP)/SCALE)

            ELSE IF (ICOPLANE.EQ.4) THEN
C
C     Constant azimuth scan (RHI) : interpolation to cartesian (x,y,z)
C     AZC - equivalent azimuth angle in vertical scan plane.  Elevation
C     is measured counter-clockwise whereas AZC is measured clockwise.  
C     Note: These "negative azimuth" angle beams must be discarded in 
C           order for the interpolation to work.  Negative azimuths are
C           not allowed in the TRPVOL interpolation code.
C
               AZC=90.0 - IBUF(34)*SCALE
               IF (AZC.LT.0.05 .OR. AZC.GT.89.95) THEN
                  BDBEAMS=BDBEAMS+1
                  GOTO 1050
               END IF
               AZ=IBUF(33)*SCALE + AZCOR
               IF(AZ.LT.0.0)AZ=AZ+360.0
               KOUT(1)=NINT(AZ*FLOAT(JRH6))
               AZZ=AZ
            ELSE
               WRITE(*,*)'***INVALID ANGLE MODE IN UFNCAR***'
               STOP
            END IF
C     
C     NORMALIZE ELEVATION ANGLE and CHECK FOR NEGATIVES
C        Store information in KOUT(2-5)
C
            IBUF(34)=INORM(IBUF(34))
            IBUF(34)=INANG(IBUF(34))
            KOUT(2)=IBUF(34)*SCALE*KOUT(7)

            IDAY=10000*IBUF(26)+100*IBUF(27)+IBUF(28)

            KOUT(3)=IBUF(29)+LFTIM
            IF (KOUT(3).LT.0) KOUT(3)=KOUT(3)+24

C     Increase the hour by 24 to allow for crossing 00 hour within a volume scan;
C     otherwise, the TIME field will be incorrect.  Only one day-change allowed.
C           JDAY - day of the first ray within the current volume scan
C           IDAY - day of current ray within the current volume scan
C
            IF (IDAY.NE.JDAY)THEN
               KOUT(3)=KOUT(3)+24
            END IF
c            print *,'UFNCAR: jday,iday,ihr=',jday,iday,kout(3)
            KOUT(4)=IBUF(30)
            KOUT(5)=IBUF(31)
            ID(7)=KOUT(3)
            ID(8)=IBUF(30)
            ID(9)=IBUF(31)
C     
C     PROCESS SWEEP - KEY ON FIXED ANGLE OR NUMBER OF GATES
C     
            ELCUR=ELSAV
            IF (ELCUR.GT.360.0) ELCUR=ELCUR-360.0
            IF(IBUF(34) * SCALE .LT. 0) THEN
               IBUF(36)=INORM(IBUF(36))
               IBUF(36)=INANG(IBUF(36))  
            END IF             
            ELOLD=IBUF(36)*SCALE            
            IF((ISWMTH.EQ.0 .AND. IBUF(10).NE.ISWP) .OR.
     X         (ISWMTH.NE.0 .AND. ABS(ELCUR-ELOLD).GT.FXSTOL))
     X         THEN
C
C     A NEW SWEEP BEEN DETECTED - WRAP UP PREVIOUS SWEEP
C     
c--------------debug (ljm)
               if(iswmth.eq.0)then
                  write(7,*)'UF(EOS) sweep=',iswmth,ibuf(10),iswp
               else
                  write(7,*)'UF(EOS) fixed=',iswmth,elcur,elold,fxstol
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
C     ALSO ADJUST RHI FIXED ANGLE (AZIMUTH) BY AZIMUTH CORRECTION
C
                     ELFIX=ELFIX+AZCOR
                     ID(IPTR)=NINT(ELFIX*ID(44))
                  END IF
c-----------------print *,'UFNCAR: elfix,iptr,id=',elfix,iptr,id(iptr)
                  ID(IPTR+1)=SIGN(1.0,DIR)
C
C     REVERSE DIRECTION OF SCAN FOR RHI SCANS WITHIN 
C     AZIMUTH SECTORS: 000-045, 225-315, and 315-405.
C
                  ELTMP=ELFIX
                  IF (ELTMP.GT.360.) ELTMP=ELTMP-360.

                  IF (ICOPLANE.EQ.4 .AND. 
     X              ((ELTMP.GE.000. .AND. ELTMP.LT.045.) .OR. 
     X               (ELTMP.GE.225. .AND. ELTMP.LT.315.) .OR. 
     X               (ELTMP.GE.315. .AND. ELTMP.LT.405.))) THEN
                     ID(IPTR+1)=-ID(IPTR+1)
                  END IF

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
     X                 DELEL,BEGAZ,SCANGL,AZMIN,AZMAX,DELAZ,GDBEAMS,
     X                 BDBEAMS,VNYQSWP
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
C        ISKIPFX=0  ==>  DON'T SKIP NEXT SWEEP
C        ISKIPFX=1  ==>  SKIP NEXT SWEEP
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
                  SCANGL=IBUF(33)*SCALE + AZCOR
                  IF (SCANGL.GT.360.0) SCANGL=SCANGL-360.0
                  IF (SCANGL.LT.0.0) SCANGL=SCANGL+360.0
               ELSE IF (ICOPLANE.EQ.4) THEN
C
C     RHI scans --> Cartesian
C     
                  SCANGL=IBUF(34)*SCALE
                  IF (SCANGL.LT.0.0 .OR. SCANGL.GT.90.0) THEN
                     WRITE(*,*)'***BAD ANGLE IN UFNCAR***'
                  END IF
               END IF
               BEGAZ=SCANGL
               CALL CONFLD(ELSCAN,MXSCN+MXL,0.0)
               ISWP=IBUF(10)
C     
C     NORMALIZE ELEVATION and CHECK FOR NEGATIVES
C     
               IBUF(36)=INORM(IBUF(36))
               IBUF(36)=INANG(IBUF(36))
               ELSAV=IBUF(36)*SCALE
c               IF (IPTR.GT.129 .AND. ABS((ELSAV+AZCOR)-ID(IPTR-3)/
               IF (IPTR.GT.IPTR_INT .AND. ABS((ELSAV+AZCOR)-ID(IPTR-3)/
     X              REAL(ID(44))).GT.180.0) ELSAV=ELSAV+360.0
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
               THETA=IBUF(34)*SCALE
               PHI=IBUF(33)*SCALE + AZCOR - (ANGXAX-90)
               IF (PHI.LT.0.0) PHI=PHI+360.0
               IF (PHI.GT.360.0) PHI=PHI-360.0
               IF (THETA.EQ.90.0) THEN
                  WRITE(*,*)'***BAD ANGLES IN UFNCAR***'
                  STOP
               END IF
C     
C     ELANG IS THE ELEV ANGLE CALCULATED FROM THE FIXED AND AZIM ANGLES
C        IBUF(33) - Scaled integer azimuth ( x CF)
C        IBUF(34) - Scaled integer elevation ( x CF)
C        IBUF(36) - Scaled integer fixed angle ( x CF)
C     
               IF (ICOPLANE.EQ.1 .OR. ICOPLANE.EQ.2) THEN
C
C     Coplane scans (R,A,C) --> COPLANE
C
                  ELANG=CALEL(IBUF(36)*SCALE,PHI)
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
                  PHI = IBUF(33)*SCALE + AZCOR - BASANG
                  IF (PHI.LT.0.0) PHI=PHI+360.0
                  IF (PHI.GT.360.0) PHI=PHI-360.0
                  ELANG = CALEL(IBUF(36)*SCALE,PHI)
                  IF (ELANG.LT.0.0 .OR. ELANG.GT.90.0) THEN
                     WRITE(*,*)'***INVALID ELEV ANGLE IN UFNCAR***'
                     STOP
                  END IF
               ELSE IF (ICOPLANE.EQ.4) THEN
C
C     RHI scans --> CARTESIAN [AZCOR corrects (rotates) azimuth angles to 
C                      conform to the user-specified azimuth for +X-axis).
C
                  ELANG=IBUF(33)*SCALE + AZCOR
               END IF
C     
C     Check tolerance between nominal (FXANG) and actual fixed angle
C        [PPI - IBUF(35) or RHI - IBUF (34)]
C        33 - Current azimuth angle (degrees * CF)
C        34 - Current elevation angle (degrees * CF)
C        36 - Fixed angle (degrees * CF)
C     
               IF (ICOPLANE.EQ.0) THEN
                  IF (ABS((IBUF(36)-IBUF(34))*SCALE).GT.ELTOL) THEN
                     BDBEAMS=BDBEAMS+1
C     
C     GO GRAB ANOTHER BEAM
C
                     GOTO 1050
                  END IF
               ELSE IF (ICOPLANE.GE.1 .AND. ICOPLANE.LE.3) THEN
                  IF (ABS((IBUF(34)*SCALE)-ELANG).GT.ELTOL) THEN
                     BDBEAMS=BDBEAMS+1
C     
C     GO GRAB ANOTHER BEAM
C     
                     GOTO 1050
                  END IF
               ELSE IF (ICOPLANE.EQ.4) THEN
                  IF (ABS((IBUF(36)*SCALE+AZCOR)-ELANG).GT.ELTOL) THEN
                     BDBEAMS=BDBEAMS+1
C     
C     GO GRAB ANOTHER BEAM
C     
                     GOTO 1050
                  END IF
               END IF
            END IF
C     
C     NORMALIZE ELEVATION and CHECK FOR NEGATIVES
C     
            IBUF(34)=INORM(IBUF(34))
            IBUF(34)=INANG(IBUF(34))
            ACTEL=IBUF(34)*SCALE
            NRAYS=NRAYS+1
            GDBEAMS=GDBEAMS+1
            IF (ICOPLANE.GE.1 .AND. ICOPLANE.LE.3) THEN
C
C     Coplane scan: interpolation to coplane (x,y,c) or cartesian (x,y,z)
C
               AZTMP=IBUF(33)*SCALE + AZCOR
               IF (AZTMP.GT.360.0) AZTMP=AZTMP-360.0
               IF (AZTMP.LT.0.0)   AZTMP=AZTMP+360.0
               DIF=AZTMP - SCANGL
               SCANGL=AZTMP
               IF (ACTEL.GE.90.0 .OR. (SCANGL - BASANG).EQ.0.0) THEN
                  WRITE(*,*)'***INVALID ANGLE IN UFNCAR***'
                  STOP
               END IF
               ELSCAN(GDBEAMS)=CALCOP(ACTEL,(SCANGL - DASANG))
               IF (ELSCAN(GDBEAMS).LT.0.0 .OR. 
     X             ELSCAN(GDBEAMS).GT.90.0) THEN
                  WRITE(*,*)'***INVALID ANGLE IN UFNCAR***'
                  STOP
               END IF
               IF (ELSCAN(GDBEAMS).GT.ELMAX) ELMAX=ELSCAN(GDBEAMS)
               IF (ELSCAN(GDBEAMS).LT.ELMIN) ELMIN=ELSCAN(GDBEAMS)
               ELSUM=ELSUM+ELSCAN(GDBEAMS)
            ELSE IF (ICOPLANE.EQ.0) THEN
C
C     Normal ppi/sur scan: interpolation to cartesian (x,y,z)
C
               AZTMP=IBUF(33)*SCALE + AZCOR
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
               AZTMP=IBUF(34)*SCALE
               DIF=AZTMP-SCANGL
               SCANGL=AZTMP
               ELSCAN(GDBEAMS)=IBUF(33)*SCALE + AZCOR
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
            LCONT=INTCHK(IBUF,RUSR2.GT.0.0)
            IF(LCONT.GT.0) CALL CHKMSG(9,NRAYS)
            IPTD=IBUF(5)
            IPTF=IBUF(IPTD+4)

C     STORE # OF RANGE GATES FOR THIS BEAM IN KOUT(8)
            IF (RUSR2.EQ.0.0) THEN
               KOUT(8)=MIN(IBUF(IPTF+5),MAXRNG)
               NRG=KOUT(8)
            ELSE
               KOUT(8)=ID(34)
            END IF
            IRGFG=INORM(IBUF(IPTF+2))
            IADFG=INORM(IBUF(IPTF+3))
            RJ1=IRGFG+IADFG*0.001
            IF (RNOTUS.NE.0.0) RJ1=RJ1+RNOTUS
            NGFH=IBUF(IPTF+5)
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
            IF (IBUF(IPTF+4).NE.ID(33) .AND. DRGUS.EQ.0.0) THEN
C
C     RANGE GATE SPACING HAS CHANGED
C
               IDRGCHG=IDRGCHG+1
               NRAYS=NRAYS-1
               GDBEAMS=GDBEAMS-1
               BDBEAMS=BDBEAMS+1
               GOTO 1050
            END IF
            IF (IBUF(IPTF+5).NE.ID(34) .AND. (DRGUS.EQ.0.0 .AND.
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
               FXANG=IBUF(36)*SCALE
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
            IUFPTR=IPTD+3
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
            IFPTRSV=IUFPTR
            
 280        IFLDPTR=IBUF(IUFPTR+1)
            WRITE(NAMPRE,419)IBUF(IUFPTR)
            IEDFLD=IBUF(IFLDPTR+16)

C     Compose a name from the two-character input UF field name
C     or the two-character field name + two-character edit code 
C
            IF(IEDFLD.EQ.32768.OR.IEDFLD.EQ.0.OR.IEDFLD.EQ.IEDBD1) THEN
               WRITE(NAMFLD,283)IBUF(IUFPTR)
 283           FORMAT(A2,'  ')
            ELSE
               WRITE(NAMFLD,407)IBUF(IUFPTR),IBUF(IFLDPTR+16)
            END IF
            IF(MFBM.EQ.MFTOL) THEN
C     
C     SUSPECTED MISSING FIELD NAMES
C     
               PRINT 285, N,NAMFLD,NAMPRE
 285           FORMAT(8X,I2,3X,A4,6X,A2,7X)
            END IF
            WRITE(CFIELD,290)NAMFLD
 290        FORMAT(A4)
            DO 330 I=1,NTHRSH
               IF (CFIELD.EQ.TFIELD(2,I)) THEN
C     Get field type from FUNCTION ITPFLDC
                  ITYP=ITPFLDC(NAMFLD)
                  ISCL=IBUF(IFLDPTR+1)
C     
C     SPECIAL MOD FOR CHILL RADAR DURING CCOPE
C     
                  FACT=1.0
                  IF (ITYP.EQ.3 .AND. 
     X                NMRAD.EQ.'CHIL' .AND. IPROJ.EQ.'CCOP') THEN
C     Rescale velocity field and reverse sign
                     FACT=-100.0/IBUF(IFLDPTR+1)
                  ENDIF
                  IPF=IBUF(IFLDPTR)
                  NGFLD=IBUF(IFLDPTR+5)
                  K=IUNPAD
                  DO 300 J=1,NRG
                     THVAL(INUM,J)=IBAD
                     IF (J.LT.J1 .OR. J.GT.J2) GOTO 300
                     IF(J.GT.NGFLD) GO TO 300
                     IF (IBUF(IPF+K).EQ.IBUF(45)) GOTO 295
                     X=IBUF(IPF+K)
                     Y=CVMGP(NINT(X-65536.),NINT(X),NINT(X-32768.))
     X                    *FACT/ISCL
                     THVAL(INUM,J)=Y
 295                 CONTINUE
                     K=K+1
 300              CONTINUE
C     
C     STORE THE NAMES OF THE FIELDS ON WHICH WE WILL THRESHOLD
C     
                  WRITE(THON(INUM),305)NAMFLD
 305              FORMAT(A4)
                  INUM=INUM+1
                  IF (INUM.GT.3) THEN
                     WRITE(*,*)'***TOO MANY THRESHOLD FIELDS IN ',
     X                    'UFNCAR***'
                     STOP
                  END IF
                  GOTO 340
               ENDIF
 330        CONTINUE
 340        CONTINUE
            IUFPTR=IUFPTR+2
            N=N+1
C     
C     CONTINUE LOOPING THROUGH MORE FIELDS, IF ANY
C     
            IF (N.LE.IBUF(IPTD+2)) GOTO 280
            N=1
            IUFPTR=IFPTRSV
 400        CONTINUE
            IFLDPTR=IBUF(IUFPTR+1)
            WRITE(NAMPRE,419)IBUF(IUFPTR)
            IEDFLD=IBUF(IFLDPTR+16)
            IF(IEDFLD.EQ.32768.OR.IEDFLD.EQ.0.OR.IEDFLD.EQ.IEDBD1) THEN
               WRITE(NAMFLD,283)IBUF(IUFPTR)
            ELSE
               WRITE(NAMFLD,407)IBUF(IUFPTR),IBUF(IFLDPTR+16)
 407           FORMAT(2A2)
            END IF
            IF(MFBM.EQ.MFTOL) THEN
C     
C     SUSPECTED MISSING FIELD NAMES
C     
c--------------PRINT 285, N,NAMFLD,NAMPRE
            END IF
C
C     Fill KOUT with field values and write it to disk in 
C     WRRYDK.
C
            DO 500 I=1,NFLINP
               WRITE(CTEMP1,409)IFIELD(I)
 409           FORMAT(A8)
               IF (NAMFLD.EQ.CTEMP1) THEN
                  READ (NAMFLD,411)ID(IDPTR),ID(IDPTR+1)
 411              FORMAT (2A4)
                  IFC=IFC+1
                  MTFIEL(IFC)=I
C     Get field type from FUNCTION ITPFLDC
                  ITYP=ITPFLDC(NAMFLD)
                  ID(IDPTR+4)=IBUF(IFLDPTR+1)
C     
C     SPECIAL MODs FOR CHILL RADAR DURING CCOPE
C     
                  FACT=1.0
                  IF (ITYP.EQ.3 .AND. 
     X                NMRAD.EQ.'CHIL' .AND. IPROJ.EQ.'CCOP') THEN
C     Rescale velocity field and reverse sign
                     ID(IDPTR+4)=100
                     FACT=-100.0/IBUF(IFLDPTR+1)
                  ENDIF
                  ID(IDPTR+2)=0
                  IF (ITYP.EQ.1) THEN
C     Received power field
                     IF (CFAC1.EQ.0.0) THEN
                        ID(IDPTR+2)=
     X                     IBUF(IFLDPTR+19)*100.0/IBUF(IFLDPTR+1)
                     ELSE
                        ID(IDPTR+2)=CFAC1*100.
                     ENDIF
                  ELSE IF (ITYP.EQ.3) THEN
C     Velocity field
                     IF (VNYQ.EQ.0.0) THEN
                        ID(IDPTR+2)=
     X                       IBUF(IFLDPTR+19)*100.0/IBUF(IFLDPTR+1)
                     ELSE
                        ID(IDPTR+2)=VNYQ*100.
                     ENDIF
                     VSCL=FLOAT(IBUF(IFLDPTR+1))
                     VNYQSWP=FLOAT(IBUF(IFLDPTR+19))/VSCL
                     WRITE(CTEMP3,419)IBUF(IFLDPTR+20)
 419                 FORMAT(A2)
                     IF(CTEMP3.EQ.MFL) ID(IDPTR+2)= -ID(IDPTR+2)
                  ENDIF
                  IPF=IBUF(IFLDPTR)
                  NGFLD=IBUF(IFLDPTR+5)
                  K=IUNPAD
C     
C     CHECK FOR REPLACEMENT OF A FIELD BY AN ANALYTIC FUNCTION
C     
                  WRITE(CTEMP3,409)IFIELD(I)
                  IF (FNUM.EQ.0 .OR. (FNUM.GT.0 .AND. RFNAM.NE.
     X                 CTEMP3)) THEN
                     DO 460 J=1,NRG
                        KOUT(KST+J)=IBAD
                        IF (J.LT.J1 .OR. J.GT.J2) GOTO 460
                        IF(J.GT.NGFLD) GO TO 460
                        IF (IBUF(IPF+K).EQ.IBUF(45)) GOTO 440
                        X=IBUF(IPF+K)
                        Y=CVMGP(NINT(X-65536.),NINT(X),NINT(X-32768.0))
     X                       *FACT
                        KOUT(KST+J)=Y
 440                    CONTINUE
                        K=K+1
 460                 CONTINUE
                  ELSE IF (FNUM.GT.0 .AND. RFNAM.EQ.CTEMP3) THEN
C
C     REPLACE MEASURED FIELD WITH ANALYTICAL FUNCTION
C
                     CALL ANLREP(FNUM,P1,P2,P3,P4,P10,NRG,KST,J1,
     X                           J2,NGFLD,IBUF(IFLDPTR+1),K,
     X                           IBUF(34)*SCALE,IBUF(36)*SCALE,AZZ,
     X                           ICOPLANE,RJ1,DRG,XORR,YORR,ZORR)
                  ELSE
                     WRITE(*,*)'***INVALID STATE FOR REPLAC. IN UFNCAR'
                     STOP
                  END IF
                  IDPTR=IDPTR+5
c                  write(7,1770)gdbeams,azz,nrg,i,kst,idptr
c 1770             format(1x,'UFbeam: gdbeams,azz,nrg,fld#,kst,idptr=',
c     +                 i8,f8.3,i6,i4,2i8)
                  KST=KST+NRG
               ENDIF
 500        CONTINUE
            IUFPTR=IUFPTR+2
            N=N+1
            IF (N.LE.IBUF(IPTD+2)) GOTO 400
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
C THIS NEXT ARRAY ELEMENT WILL CONTAIN THE TOTAL NUMBER OF POINTS IN VOL. SCAN
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
            IF (IBUF(9).GT.1) GOTO 400
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
               WRITE(*,*)'***TOO MANY EOFs--WRAPPING UP VOLUME***'
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
                     CALL UFDUMP(IBUF,NWDS,MAXIN,NST,SCALE,IPREC,NAZZ)
                  END IF
               END IF
               IBUF(33)=INORM(IBUF(33))
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
                     CALL UFDUMP(IBUF,NWDS,MAXIN,NST,SCALE,IPREC,NAZZ)
                  END IF
               END IF
               iswp=ibuf(10)
               az=scale*ibuf(33)
               el=scale*ibuf(34)
               fx=scale*ibuf(36)
               IBUF(33)=INORM(IBUF(33))
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
                     IFUT=IBUF(32)
C                     CALL SHILBL(IFUT,1)
                     LFTIM=0
C     MARK BRADFORD PATCH TO REMOVE CONVERSION TO MDT
C     IF (IFUT.EQ.'UT') LFTIM=-6
                  ENDIF
                  IHR=IBUF(29)+LFTIM
                  IF (IHR.LT.0) IHR=IHR+24
                  IFTIM=10000*IHR + 100*IBUF(30) + IBUF(31)
                  if(ifd.eq.1)then
                     write(7,*)'UFNCAR: iftim,ketim=',iftim,ketim
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
                  IBUF(36)=INORM(IBUF(36))
                  IBUF(36)=INANG(IBUF(36))
                  CUREL=IBUF(36)*SCALE
                  IF (ABS(CUREL-OLDEL).GT.FXSTOL) THEN
                     IF (NST.NE.0) THEN
                        WRITE(*,*)'***BAD STATE IN UFNCAR***'
                        STOP
                     END IF
                     IALREAD=1
                     GOTO 1050
                  END IF
               END IF

               IKAZ=IKAZ+1
               ISWP=IBUF(10)
               IF (ICOPLANE.LT.4) THEN
                  CURAZ=IBUF(33)*SCALE + AZCOR
                  IF (CURAZ.GT.360.0) CURAZ=CURAZ-360.0
                  IF (CURAZ.LT.0.0) CURAZ=CURAZ+360.0
               ELSE IF (ICOPLANE.EQ.4) THEN
                  CURAZ=IBUF(34)*SCALE
               END IF
               IF (((CURAZ-OLDAZ)*IDIR).GT.0.0) THEN
                  IF (NST.NE.0) THEN
                     WRITE(*,*)'***BAD STATE IN UFNCAR***'
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
      write(7,*)'UF (eos): ivol,iswp,gdbeams=',ivol,iswp,gdbeams
      IF (GDBEAMS.LT.MNBEM .AND. IFXTAB.NE.1) THEN
C         CALL WRRYDK(KPCK,KOUT,INST,LTMP,-9,0)
         IF (GDBEAMS.GT.0) THEN
            IFLG=NSWPS+11
            CALL WRRYDK(KPCK,KOUT,INST,LTMP,IFLG,GDBEAMS)
c-----------debug (ljm)
            write(7,*)'UFvol (eov): nswps,gdbeams=',nswps,gdbeams
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
c--------print *,'UFNCAR: elfix,iptr,id=',elfix,iptr,id(iptr)
         ID(IPTR+1)=SIGN(1.0,DIR)
C
C     REVERSE DIRECTION OF SCAN FOR RHI SCANS WITHIN 
C     AZIMUTH SECTORS: 000-045, 225-315, and 315-405.
C
         ELTMP=ELFIX
         IF (ELTMP.GT.360.) ELTMP=ELTMP-360.

         IF (ICOPLANE.EQ.4 .AND. 
     X     ((ELTMP.GE.000. .AND. ELTMP.LT.045.) .OR. 
     X      (ELTMP.GE.225. .AND. ELTMP.LT.315.) .OR. 
     X      (ELTMP.GE.315. .AND. ELTMP.LT.405.))) THEN
            ID(IPTR+1)=-ID(IPTR+1)
         END IF

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
     X        BEGAZ,SCANGL,AZMIN,AZMAX,DELAZ,GDBEAMS,BDBEAMS,VNYQSWP
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
 2000  CONTINUE
C     
C     VOLUME IS FINISHED
C     
      IFLG=9
      CALL WRRYDK(KPCK,KOUT,INST,LTMP,IFLG,0)
c-----debug (ljm)
      write(7,*)'UFvol: End-of-volume'
c-----debug (ljm) 
      IF (NSWPS.LT.2 .AND. IPPI.EQ.0) THEN
         write(7,*)'UFNCAR: irov=',irov
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
     X                'UNIFORMLY SPACED. REINTERPOLATE AND SPECIFY',
     X                'TARGET ANGLES EXPLICITLY WITH GRIDCPL CARD. ')
               STOP 2015
            END IF
         END IF
         IF(ID(K).LE.LAST) THEN
            PRINT *,ID(K), LAST
            PRINT *,"IN HERE ILSCHK IS TRUE"
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
      print *,'UF: ielchk,ilschk=',ielchk,ilschk
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
C     
C     PRINT OUT NUMBER OF DATA POINTS SET TO BAD DUE TO THRESHOLDING
C     
      WRITE(*,*)' '
      IF (NTHRSH.GT.0) THEN
         WRITE(*,2041)
 2041     FORMAT(15X,'POINTS SET TO BAD DUE TO INSIDE/OUTSIDE TESTS')
         WRITE(*,2044)
 2044     FORMAT(15X,'------ --- -- --- --- -- -------------- -----')
         WRITE(*,2047)
 2047    FORMAT(/,10X,'FIELD        % SET TO BAD  # SET TO BAD  ',
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
      NUFST=0
C
C     WRITE OUT THE NUMBER OF BEAMS DISCARDED DUE A CHANGE IN GATE
C     SPACING AND THE NUMBER THROWN OUT DUE TO A CHANGE IN THE # OF GATES
C
      IF (IDRGCHG.GT.0) THEN
         WRITE(*,2059)IDRGCHG
 2059    FORMAT(/,I5,' BEAMS DISCARDED DUE TO A CHANGE IN THE GATE ',
     X          'SPACING',/)
      END IF
      IF (INRNG.GT.0) THEN
         WRITE(*,2060)INRNG
 2060    FORMAT(/,I5,' BEAMS DISCARDED DUE TO A CHANGE IN THE # OF ',
     X          'RANGE GATES',/)
      END IF
         
      IF (ICOPLANE.EQ.4 .AND. ANGXAXT.NE.90.0) THEN

C     RESET AZIMUTH CORRECTION BACK TO ORIGINAL VALUE

         AZCOR=AZCORT
         ANGXAX=ANGXAXT
      END IF

      RETURN
      END
