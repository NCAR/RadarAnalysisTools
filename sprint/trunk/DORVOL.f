      SUBROUTINE DORVOL(NUFST,JPCK,ELSCAN,ICRTST,CTDBM,CTDBMXH,CTDBMXV,
     X     NTHRSH,TFIELD,TLIMITS,RFNAM,FNUM,P1,P2,P3,P4,P10,AZCOR,
     X     DASANG,ISIDE,USER_DEF_ORIGIN,RADAR_CHOSEN,KOMM,VNYQUIST,
     X     IFD,IFD_RAYS)
C     
C     THIS SUBROUTINE READS ONE VOLUME SCAN IN DORADE FORMAT
C     AND OUTPUTS IT TO DISK IN A SIMPLIFIED FORMAT.
C
C     ITP - scan mode returned from RDBEAM
C
C        ITP < 0: Invalid scan mode
C        ITP = 0: Calibration
C        ITP = 1: PPI (constant elevation)
C        ITP = 2: Coplane
C        ITP = 3: RHI (constant azimuth)
C        ITP = 4: Vertical pointing
C        ITP = 5: Target (stationary)
C        ITP = 6: Manual
C        ITP = 7: Idle (out of control)
C        ITP = 8: Full sweep (surveilance?)
C        ITP = 9: Partial sweep (airborne?)
C        ITP > 9: Unknown scan mode
C
C     RADAR_TYPE - radar type returned from RDBEAM
C
C        RADAR_TYPE < 0: Unknown radar type
C        RADAR_TYPE = 0: Ground based
C        RADAR_TYPE = 1: Airborne for
C        RADAR_TYPE = 2: Airborne aft
C        RADAR_TYPE = 3: Airborne tail
C        RADAR_TYPE = 4: Airborne lower fuselage
C        RADAR_TYPE = 5: Shipborne
C        RADAR_TYPE > 5: Unknown radar type
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
C     Significant ID values:
C        Set here:
C             4  - Initial hour for the current scan volume
C            14  - Radar name (A2)
C            15  - Radar name (A2)
C            16  - Radar name (A2)
C            35  - Sweep counter
C            36  - Number of rays in this sweep
C           IPTR - Initial value is IPTR_INT
C            ***** Airborne (ICOPLANE=5):
C             +0 - Mean value for aircraft TRCK
C             +1 - Mean value for aircraft Drift
C             +2 - Number of rays (beams)
C            ***** Ground-based (ICOPLANE=0-4):
C             +0 - Fixed angle of current volume scan (DORSWP)
C             +1 - Scan direction for current sweep
C             +2 - Total number of rays (beams)
C        Set in INITVOL:
C            31  - See RNGFIL: (RG1) Adjusted range to first gate (m)
C                  RG1 = R0 + RNOTUS = data + user-supplied correction
C                  ID(31)=INT(RG1)
C            32  - ID(32)=NINT((RG1-ID(31))*1000.0)
C            33  - Range gate spacing (m)
C            47  -  100*X-coordinate for the radar
C            48  -  100*Y-coordinate for the radar
C            46  - 1000*Z-coordinate for the radar
C
C     ANGXAX = ANGLE OF X AXIS FROM TRUE NORTH (USUALLY 90.0)
C     BASANG = ANGLE OF Y AXIS FROM TRUE NORTH; USED IN COPLANE SCANS
C     DASANG = ANGLE OF Y AXIS FROM BASELINE OF RADARS; USED IN COPLANE SCANS
C        Values of BASANG and DASANG:
C           - Not used for non-coplane scans
C           - Same value when interpolating from coplane scans to coplane
C           - May be different when interpolating from coplane scans to xyz
C     
C     RFNAM - Field name which will be replaced with analytic function
C     FNUM  - Analytic field number (1-15, set in ANALYT)
C     
C     Note: KDAY - user requested day to be processed (set in PROFIL)
C           JDAY - day of the first ray within the current volume scan
C           IDAY - day of the current ray of data of the current volume scan
C           If the current day changes from the 1st ray day, add 24 to hours.
C
      INCLUDE 'SPRINT.INC'
c      PARAMETER (MAXSKP=27,MXCNT=500)

      DIMENSION JPCK(1),ELSCAN(1),IOVER(6),ISIDE(MAXFLD)
      DIMENSION TLIMITS(2,MAXFLD),ITHR(MAXFLD+1)
      DIMENSION CTDBM(MXCNT),CTDBMXH(MXCNT),CTDBMXV(MXCNT)
      CHARACTER*8 KRD(10),RFNAM,P10
      CHARACTER*8 CTEMP1
      CHARACTER*8 TFIELD(2,MAXFLD)
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
      
      CHARACTER*8 IDCHAR(50)

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
      
      CHARACTER*8 RADNAM,FLTNUM,FLDNAM(MXFDOR)
      DIMENSION FLDDAT(MXGDOR,MXFDOR)

      CHARACTER*4 PROJ_NAME

      COMMON /BYTORD/ MBYTE,SWAPPING
      COMMON /SCRDSK/ LTMP
      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI,ILLE,ILLZ
      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X     ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      COMMON /INITV/ NRNG,RMIN,GATSPAC,INIT_MIN,INIT_SEC
      LOGICAL IELCHK,ILSCHK
      COMMON /AIRBRN/ ALATS(MAXEL),ALONS(MAXEL),IALTFLG,THE_TILT
      REAL THE_TILT,TILTS(MAXEL)
      REAL ALAT,ALON
      REAL GRNDSPDEW,GNDSPDNS,VERVEL
      REAL DIR
      INTEGER TILTFLG
      DIMENSION ALTMEAN(MAXEL), TRCKMEAN(MAXEL),DRFTMEAN(MAXEL)
      DIMENSION PTCHMEAN(MAXEL), ROLLMEAN(MAXEL)
      DIMENSION VNYQUIST(MAXEL)

      CHARACTER*3 KOMM
      CHARACTER*8 INP,IOUT,MET,ICRT,IEMPTY,NAMTIM
      INTEGER RADAR_TYPE
      INTEGER THEMODE(1000),MODEVALUE
      INTEGER PREVIOUS_FIXED_ANGLE
      SAVE ISTAT
      INTEGER END_SWEEP_FILE,SWAPPING
      INTEGER TOTAL_NUMBER_RAYS
      INTEGER FNUM
      DATA INP,IOUT,MET,ICRT/'INPUT','OUTPUT','INTERP','GRID'/
      DATA IEMPTY/'-9999'/
      DATA MFTOL/25/
      DATA EPS/0.15/
      DATA IRYSTAT/0/

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
C     IROV set by PROCESS command (see PROFIL routine).
C     Note: Use RUNOVER for processing DORADE sweep files.
C        IROV  =  (-1) APPEND, (0) NORMAL PROCESSING, (1) RUNOVER
C        ICOMBN = (+1) APPEND, (0) NORMAL PROCESSING, (2) RUNOVER

      IF (IROV.EQ.-1)ICOMBN=1
      IF (IROV.EQ. 0)ICOMBN=0
      IF (IROV.EQ. 1)ICOMBN=2

C
C      Initialize values of IRYSTAT and swapping
      
c-----print *,'DORVOL: initial value of irystat  = ',irystat
c-----print *,'DORVOL: initial value of swapping = ',swapping

c-----debugging statements (ljm)
      write(7,*)'DORVOL: nflds,irov,icombn=',nflds,irov,icombn
      write(7,*)'DORVOL:  KDAY,KBTIM,KETIM=',kday,kbtim,ketim
c-----debugging statements (ljm)

      ILSCHK=.FALSE.
 2    IFRST  =1
      ANGXAXT=90.
      IDRGCHG=0
      INRNG  =0
      IEOF   =0
      IFLGBAS=0
      BASANG =ANGXAX-90.0
c      IPTR   =129
      IPTR   =IPTR_INT
      ELTOL = AMIN1(ELTUS,FXSTOL)

      NFLINP =NFLDS
      IF(IFIELD(NFLDS).EQ.NAMTIM) NFLINP=NFLINP-1
      ALATS(1)=0.0
      ALONS(1)=0.0
      PREVIOUS_FIXED_ANGLE = 0
      TOTAL_NUMBER_RAYS = 0 
      END_SWEEP_FILE = 0

      DO J = 1,1000
         THEMODE(J) = 0
      END DO
C     
C     CALCULATE RADAR COORDINATES IN ROTATED (IF ROTATED) COORD. SYS.
C     
      write(7,*)'DORVOL - Rotated XYZ: angxax,id(47,48,46)=',
     X     angxax,id(47),id(48),id(46)
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
C     
C     CHECK TO SEE IF ALL COMMONS ARE FILLED
C     
      IF (ITAP.NE.IEMPTY) GOTO 5
      PRINT 105,INP
      STOP
 5    CONTINUE
      IF (LTAP.NE.IEMPTY) GOTO 10
      PRINT 105,IOUT
      STOP
 10   CONTINUE
 20   CONTINUE
      IF (ISCI.NE.IEMPTY) GOTO 25
      PRINT 105,NNFO
      STOP
 25   CONTINUE
      IF (ICRTST.GE.0) GOTO 60
      PRINT 105,ICRT
      STOP
C     
C     OTHER CHECKS GO HERE
C     
 105  FORMAT(5X,'+++  ',A8,' COMMAND MUST APPEAR BEFORE THE PROCESS',
     X     ' COMMAND  +++')
 60   CONTINUE
      NFLINP =NFLDS
      IF(IFIELD(NFLDS).EQ.NAMTIM) NFLINP=NFLINP-1
 200  CONTINUE
C     
C     START      - FIND VOLUME => TRANSLATE
C     
C     INITIALIZE THRESHOLD COUNTERS
      DO 23 I=1,MAXFLD+1
         ITHR(I)=0
 23   CONTINUE
C     
C     DETERMINE THE METHOD OF ELEVATION ANGLE COMPUTATION
C     
      WRITE (CTEMP1,550)IRCFXL
 550  FORMAT(A8)
      READ (CTEMP1,232)I,GNEL
 232  FORMAT(A2,1X,F5.0)
      IF(CTEMP1(1:2).EQ.'MD') THEN
C     MODE
         IELT=1
         IGNEL=AMAX1(GNEL,0.0)
         IELCHK=.TRUE.
      ELSE IF(CTEMP1(1:2).EQ.'MN') THEN
C     MEAN
         IELT=2
         IGNEL=AMAX1(GNEL,0.0)
         IELCHK=.TRUE.
      ELSE
C     TAPE VALUE
         IELT=0
         IGNEL=0
         IELCHK=.FALSE.
      END IF
C     
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
      WRITE(LTMP) LTMP
      REWIND LTMP
      CALL WRRYDK(KPCK,KOUT,INST,LTMP,IFLG,0)
c     subroutine WRRYDK(KPKBUF,KUNBUF,NST,IUN,IFLG,NLEN)
c-----debug (ljm) - eor/uio: "off end of record" in WRRYDK at READ(IUN)
c     Fix that worked was to restore RDRY common block in RDRYDK.
      jst=inst
      mlen=0
      write(7,*)'DORint: unit,jst,iflg,len,plen=',
     +     ltmp,jst,iflg,mlen,ilstrec
c-----debug (ljm)

      NSWPS=0
      NRAYS=0
      REQTIME = KBTIM
      ICHNGNAM = 1
C
C     NMRAD: User-requested radar name
C
      IF (NMRAD.EQ.'ELDA') THEN
         RADNAM='TA-ELDR'
      ELSE IF (NMRAD.EQ.'ELDF') THEN
         RADNAM='TF-ELDR'
      ELSE IF (NMRAD.EQ.'CP-2') THEN
         RADNAM='NONE'
         ICHNGNAM = 1
      ELSE
         RADNAM=NMRAD
      END IF
C
C     Get to right volume by time (KBTIM).   No other positioning will be 
C     done since requested time will be set back to ZERO to process remaining
C     beams and sweep files within this PROCESS with RUNOVER sequence.
C
c-----(LJM 7/15/09) Problem with irystat passed back from dorade.c
c-----has been fixed
c-----print *,'DORVOL - Before RDBEAM:      iun = ',iun
c-----print *,'DORVOL - Before RDBEAM:  irystat = ',irystat
c-----print *,'DORVOL - Before RDBEAM: swapping = ',swapping

 50   CALL RDBEAM(IUN, IREWND, ISTAT, IVOL, IYR,IMON,IDAY, 
     X     IHR, IMIN, ISEC, MSEC, NUMRADS, ITP,NFLD,
     X     NUMFREQ, NUMIPP, NRNG,ISWP,JULDAY,IRYSTAT,
     X     RADAR_TYPE,REQTIME,RADCON, SCANRATE, ALON, ALAT,
     X     VNYDAT,RNGMX,RMIN, GATSPAC,AZ,EL,ALTMSL,PRESALT,
     X     ALTGND,GRNDSPDEW,GNDSPDNS,VERVEL,HEADING,ROLL,
     X     PITCH,DRIFT,ROTANG,TILT,UAIR,VAIR,WAIR,
     X     HEDCHGRT,PITCHGRT,FLDDAT,BAD,FXANG,RADNAM,
     X     FLDNAM,PROJ_NAME,FLTNUM,SWAPPING)

C     Patch to make this Eldora fligt be a straight-line patch
C
      heading=360.0
      roll=0.0
      pitch=0.0
      drift=0.0
      tilt=15.8
C
C     Status of the ray information block (ryib.status in dorade.c)
C     IRYSTAT =  0 --> Normal (good) ray information block (RYIB)
C     IRYSTAT =  1 --> Transition ray information block (RYIB).
C                      Sometimes this is incorrect in dorade files.
C     IRYSTAT =  2 --> Bad ray information block (RYIB), rare
C
C     ISTAT =  0 --> A successful read of a beams worth of data (many fields)
C     ISTAT =  1 --> An end to a sweep containing several beams of data (SWIB)
C     ISTAT =  2 --> Beginning of a new volume (VOLD)
C     ISTAT = >2 --> A "read error" of any kind such as END OF DATA MEDIA,
C                     CAN'T FIND RADAR, ETC.
C     ISTAT = -2 --> End of a volume scan (VOLD)
C     ISTAT = -3 --> An end of a sweep from a sweep file (NULL)
C
C     Set JSTAT according to ISTAT:
C        JSTAT = 1  --> End of current sweep
C        JSTAT = 2  --> End of current volume
C        JSTAT = 3  --> EOD or I/O ERROR
C

C     S-Pol: Skip beams at the beginning of the lowest fixed angle scan until 
C     the antenna is rotating the same direction as the remainder of the scan.
C     For example, antenna is at the correct fixed angle but has not reached
C     the left or right azimuth boundary of the sector.
C
c-----------------------------------------------------------------
c      write(6,1767)'DORVOL#1:',totbeams,itp,fxang,az,el,dir,icoplane
c 1767 format(A9,' beams,itp,fx,az,el,icoplane=',2i8,4f10.3,i8)

c-----print *,'DORVOL - After RDBEAM:      iun = ',iun
c-----print *,'DORVOL - After RDBEAM:  irystat = ',irystat
c-----print *,'DORVOL - After RDBEAM: swapping = ',swapping

      IF(RMIN.LT.0.0)RMIN=0.0
      IFTIM=10000*IHR + 100*IMIN + ISEC
      IF (IFTIM.LT.KBTIM) THEN
         PRINT 53, IDAY,IFTIM,KBTIM
 53      FORMAT(' +++  BEAM WILL BE SKIPPED  -  DAY: ',I6,
     X        5X,'BEGINNING TIME: ',I6,' BEFORE REQUESTED TIME: ',
     X        I6,'  +++')
         NSWPS=0
         NRAYS=0
         GOTO 50
      ELSE
         ISTAT = 2
      END IF
c-----------------------------------------------------------------
      if(ifrst .eq. 1)then
         print *,'DORVOL: nmrad,radnam,type=',nmrad,' ',radnam,' ',
     X        radar_type
         print *,'DORVOL: scan mode,icoplane=',itp,icoplane
      end if
C
C     Set coplane flag (ICOPLANE) if Airborne (for, aft, or tail)
C
      IF(RADAR_TYPE .EQ. 1 .OR. 
     X   RADAR_TYPE .EQ. 2 .OR. 
     X   RADAR_TYPE .EQ. 3)THEN
         ICOPLANE = 5
      END IF
C
C     SCAN MODE - CHECK SCAN MODE AND SET ICOPLANE, IF NECESSARY
C
      IF(ICOPLANE .NE. 5)THEN
         IF (ITP .EQ. 1)THEN
            ICOPLANE = 0
         ELSE IF (ITP .EQ. 2)THEN
            PRINT *,
     X      '++++ INTERPOLATION OF COPLANE SCANS NOT IMPLEMENTED ++++'
            PRINT *,
     X      '++++ ONLY PPI AND RHI SCAN MODES CAN BE INTERPOLATED ++++'
            PRINT *,'++++ THIS RADAR SCAN MODE - ITP=',ITP,' ++++'
            STOP
c------------(ljm) leave commented until coplane scans are implemented
c            IF (ICOPLANE .LT. 1 .OR. ICOPLANE .GT. 3)THEN
c               PRINT *,
c     X         '++++ COPLANE SCAN WITH INCORRECT OUTPUT GRID SPECS ++++'
c               PRINT *,
c     X         '           ++++ CHECK YOUR GRID COMMAND ++++           '
c               STOP
c            END IF               
c------------(ljm) leave commented until coplane scans are implemented
         ELSE IF (ITP .EQ. 3)THEN
            ICOPLANE = 4
         ELSE IF (ITP .EQ. 8)THEN
            ICOPLANE = 0
         ELSE IF (ITP .EQ. 9)THEN
            ICOPLANE = 0
         ELSE
            PRINT *,
     X      '++++ ONLY PPI AND RHI SCAN MODES CAN BE INTERPOLATED ++++'
            PRINT *,'++++ THIS RADAR SCAN MODE - ITP=',ITP,' ++++'
            STOP
         END IF
      END IF

      IF(IFRST.EQ.1)THEN
         IF (ICOPLANE.EQ.0) THEN
            WRITE(*,2000)
 2000       FORMAT(5X,'+++GRIDDING DATA IN R,A,E TO 3-D CARTESIAN+++')
         ELSE IF (ICOPLANE.GT.0 .AND. ICOPLANE.LT.3) THEN
            DASANG=BASANG
            WRITE(*,2010)
 2010       FORMAT(5X,'+++GRIDDING DATA IN R,A,C, TO REGULAR COPLANE ',
     X           'GRID+++')
         ELSE IF (ICOPLANE.EQ.3) THEN
            IF (DASANG.EQ.-999.0)DASANG=BASANG
            WRITE(*,2020)
 2020       FORMAT(5X,'+++GRIDDING DATA IN R,A,C TO 3-D CARTESIAN+++')
         ELSE IF (ICOPLANE.EQ.4) THEN
            WRITE(*,2022)
 2022       FORMAT(5X,'+++GRIDDING DATA IN RHI TO 3-D CARTESIAN+++')
         END IF
         print *,'DORVOL: icoplane=',icoplane
         print *,'DORVOL: angxax,basang,dasang=',angxax,basang,dasang
         IF(ICOPLANE .NE. 0 .AND. 
     X      ICOPLANE .NE. 4 .AND.
     X      ICOPLANE .NE. 5)THEN
            PRINT *,
     X           '++++ ONLY PPI, RHI, OR AIRBORNE',
     X           ' SCAN MODES CAN BE INTERPOLATED ++++'
            STOP
         END IF
      END IF

      IF (ICOPLANE.EQ.4 .AND. ANGXAX.NE.90.0) THEN

C     DUE TO CONTORTED WAY THAT RHIS HAVE TO BE INTERPOLATED,
C     X-AXIS ROTATIONS NEED TO BE HANDLED VIA AZIMUTH CORRECTION
C         
         AZCORT=AZCOR
         AZCOR=AZCOR+(90.-ANGXAX)
         ANGXAXT=ANGXAX
         ANGXAX=90.
         WRITE(*,85)AZCOR,ANGXAXT
 85      FORMAT(5X,'+++RHI AZIMUTHS WILL BE ROTATED ',
     X        F6.2,' DEGRESS SINCE AZIMUTH +X-AXIS SPECIFIED AS ',
     X        F6.2,' INSTEAD OF 90 DEG+++')
         PRINT *,'     +++FOR RHI SCAN -  ROTATE IN CEDRIC+++'
      END IF

      IFTIM=10000*IHR + 100*IMIN + ISEC
      IF(ISTAT.EQ.0)NRAYS=NRAYS+1

      IF(ICHNGNAM .EQ. 1) THEN
         IF(RADNAM(2:4).EQ.'POL')RADNAM(1:1)='S'
         read(radnam,30),(ID(I),I=14,16)
 30      FORMAT(3A2)
         write(7,*)'DORVOL: radnam,nmrad,ichngnam=',radnam,nmrad,
     +        ichngnam
         write(7,31)(id(i),i=14,16)
 31      format(' DORVOL:             id(14-16)=',3a2)
      END IF

c-----debugging statements (ljm)
      write(7,1769)iun,istat,irystat,nfld,kbtim,iftim,msec,istat
 1769 format(
     X     ' DORVOL - after initial rdbeam: iun,istat,irystat,nfldin=',
     X     4i4,'  kbtim,iftim=',i6,2x,i6,'.',i3.3,' istat=',i2)
      write(7,1770)rmin,gatspac,nrng
 1770 format(' DORVOL - after initial rdbeam: rmin,gatspac,nrng=',
     +     2f8.3,i8)
c-----debugging statements (ljm)

      IF(ISTAT .GT. 2) THEN

C     ISTAT = >2 --> A "read error" of any kind such as END OF DATA MEDIA,
C                     CAN'T FIND RADAR, ETC.
         JSTAT = 3
         NUFST = 2
         KOMM='???'
         PRINT *,
     X        ' +++  END OF DATA - RETURN FROM DORVOL ',
     X        'WITH JSTAT,NUFST=',JSTAT,NUFST,'  +++'
         RETURN

      ELSE IF(ISTAT .EQ. -2) THEN

C     ISTAT = -2 --> End of a volume scan (VOLD)
C
         IF(ICOMBN.NE.2)PRINT *,
     X        '+++  READ HEADER (VOLD) AT END OF VOLUME SCAN  +++'
         GOTO 50

      ELSE IF (ISTAT.EQ.2) THEN

C     ISTAT =  2 --> Beginning of a new volume (VOLD)
C                    Check data time against time requested
C
         IF(ICOMBN.NE.2)PRINT *,
     X        '+++  READ HEADER (VOLD) AT BEGINNING OF VOLUME SCAN  +++'
         IFTIM=10000*IHR + 100*IMIN + ISEC
         IF (IFTIM.LT.KBTIM) THEN
            PRINT 230, KDAY,IFTIM,KBTIM
 230        FORMAT(' +++  VOLUME WILL BE SKIPPED  -  DAY: ',I6,
     X           5X,'BEGINNING TIME: ',I6,' BEFORE REQUESTED TIME: ',
     X           I6,'  +++')
C     Why is NSWPS set to one since searching for beginning time?
            NSWPS=1
            NRAYS=0
            GOTO 50
         ELSE IF (IFTIM.GT.KETIM) THEN
            PRINT 233, IUN,IFTIM,KETIM
 233        FORMAT(/1X,'+++  UNIT: ',I2,5X,'CURRENT TIME ON TAPE: ',I6,
     X           '  IS PAST THE REQUESTED ENDING TIME TO PROCESS: ',I6,
     X           '  +++'/)
            IF(ICOMBN.EQ.0)THEN
               NUFST = 1
               JSTAT = 3
               KOMM='???'
               PRINT *,
     X              ' +++  PAST REQUESTED TIME - RETURN FROM DORVOL ',
     X              'WITH JSTAT,NUFST=',JSTAT,NUFST,'  +++'
               RETURN
            ELSEIF (ICOMBN.EQ.1)THEN
               IF (NSWPS.LT.2)  THEN
                  WRITE(*,501)NSWPS
                  GOTO 2
               END IF
            END IF
         END IF

         write(7,*)' '
         write(7,*)'DORVOL: begin a new volume, init nswps=',nswps
         
      ELSE IF (ISTAT.EQ.0 .OR. ISTAT.EQ.1) THEN

C     ISTAT =  0 --> Successful read of a beams worth of data (many fields)
C     ISTAT =  1 --> An end of sweep containing several beams of data (SWIB)
C                    Keep reading beams within the next sweep.
C
         IF (ISTAT.EQ.1)THEN
            PRINT *,'+++  END OF SWEEP NUMBER ',NSWPS,'  +++'
            NSWPS=NSWPS+1
            write(7,*)'DORVOL: #1 increment nswps=',nswps
            NRAYS=0
         END IF
         GOTO 50

      ELSE

C     ISTAT = >2 --> A "read error" of any kind such as END OF DATA MEDIA,
C                     CAN'T FIND RADAR, ETC.
C             Any kind of read error - can't find requested time 
C             and/or radar before reaching the end of data.
C
         WRITE(*,70)IUN,ISTAT
 70      FORMAT(/,5X,' +++ ERROR GETTING TO START TIME ON UNIT ',I3,
     X        ' STATUS=',I2,' +++')
         STOP
      END IF

      IF(END_SWEEP_FILE .EQ. 0) THEN

c--------debugging statements (ljm)
         if(istat.ne.0)then
            write(7,*)'DORVOL: after initial rdbeam, Unit number=',iun
            write(7,1771)iyr,imon,iday,ihr,imin,isec,msec,alat,alon,
     X           altgnd,presalt,heading,drift,track,roll,pitch,tilt,
     X           rotang,fxang,az,el
 1771    format(' ymd=',i4.4,2i2.2,' hms=',3i2.2,'.',i3.3,' ll=',
     X        f8.4,f10.4,' z(gd,pre)=',2f6.3,'  hdtrptr=',7f7.2,
     X        '  fae=',3f7.1)
            write(7,*)'Number of available fields=',nfld
            write(7,1768)(fldnam(n),n=1,nfld)
            write(7,*)'Number of requested fields=',nflds
            write(7,1768)(ifield(n),n=1,nflds)
 1768       format(5('   ',8a8,/))
         end if
c--------debugging statements (ljm)
c-----debugging statements (ljm)
         write(7,*)
     +        'DORVOL - volume found: ifrst,irov,icombn,nswps,nrays=',
     +        ifrst,irov,icombn,nswps,nrays
c-----debugging statements (ljm)

C     Found the requested time within the volume scan.
C        Set JDAY (the initial day) for all subsequent processing.
C        Reset KBTIM (the requested initial time) back to 0.
C        Set initial time of processing for output file header.
C
         IF(IFRST.EQ.0 .AND. NSWPS.NE.0 .AND. ICOMBN.NE.0)GO TO 240 
         JDAY = IDAY
         KBTIM = 0
         REQTIME = KBTIM
         INIT_HR  = IHR
         INIT_MIN = IMIN
         INIT_SEC = ISEC
         PRINT 231,KDAY,IFTIM
 231     FORMAT (//100('+')//
     X        8X,'DORVOL: VOLUME FOUND   -   DAY : ',I6,
     X        8X,'BEGINNING TIME : ',I6)
         IF(ICOMBN.EQ.0)NSWPS=0
         NRAYS=0
         IF (ICOPLANE.EQ.0) THEN
            PRINT 236
         ELSE IF (ICOPLANE.GE.1 .AND. ICOPLANE.LE.3) THEN 
            PRINT 237
         ELSE IF (ICOPLANE.EQ.4) THEN
            PRINT 238
         ELSE IF (ICOPLANE.EQ.5) THEN
            PRINT 239
         END IF
 236  FORMAT (//5X,'SCAN',26X,'ELEVATION',26X,'AZIMUTH',
     X     8X,'BEAMS'/6X,'NO',3X,'DIR',7X,'FIXED',7X,'MIN',7X,'MAX',
     X     6X,'MEAN',9X,'BEG',9X,'END',
     X     2X,'GOOD',3X,'BAD',3X,'VNYQ')
 237  FORMAT (//6X,'SCAN',18X,'COPLANE',29X,'AZIMUTH',14X,'SPACING',
     X     18X,'BEAMS'/4X,'NO',3X,'DIR',8X,'FIXED',6X,'MIN',6X,'MAX',
     X     5X,'MEAN',11X,'BEG',7X,'END',6X,'MIN',6X,'MAX',
     X     5X,'MEAN',7X,'GOOD',3X,'BAD',3X,'VNYQ')
 238  FORMAT (//6X,'SCAN',18X,'AZIMUTH',29X,'ELEVATION',14X,'SPACING',
     X     18X,'BEAMS'/4X,'NO',3X,'DIR',8X,'FIXED',6X,'MIN',6X,'MAX',
     X     5X,'MEAN',11X,'BEG',7X,'END',6X,'MIN',6X,'MAX',
     X     5X,'MEAN',7X,'GOOD',3X,'BAD',3X,'VNYQ')
c239     FORMAT(//10X,'SCAN',16X,'TRACK',14X,'ROTANG',14X,'SPACING',
 239     FORMAT(//10X,'SCAN',7X,'TRACK (HEADING+DRIFT)',7X,'ROTANG',
     X        14X,'SPACING',
     X        18X,'TILT',15X,/,'UNIT',4X,'NO',3X,'DIR',7X,'MIN',
     X        5X,'MAX',4X,'MEAN',5X,'BEG',5X,'END',5X,'MIN',5X,'MAX',
     X        4X,'MEAN',5X,'MIN',5X,'MAX',4X,'MEAN',2X,'BEAMS',
     X        1X,'ISTAT')
 240     CONTINUE

C     If PROJ_NAME from RDBEAM is all NULLS, set to user-specified IPROJ
C
         IF(PROJ_NAME .EQ. '\0\0\0\0')PROJ_NAME=IPROJ
         write(7,*)'DORVOL: proj_name,iproj=',proj_name,'|',iproj,'|'

         ID(4) = INIT_HR
c-----debugging statements (ljm)
         write(7,*)'DORVOL - before initvol: init hhmmss,id(31-34)=',
     +        init_hr,init_min,init_sec,id(31),id(32),id(33),id(34)
         write(7,*)'DORVOL - before initvol: proj_name,iproj=',
     +        proj_name,'?',iproj,'?'
c-----debugging statements (ljm)

         CALL INITVOL(IPROJ)

c-----debugging statements (ljm)
         write(7,*)'DORVOL - after initvol: init hhmmss,id(31-34)=',
     +        init_hr,init_min,init_sec,id(31),id(32),id(33),id(34)
         write(7,*)'DORVOL - after initvol: proj_name,iproj=',
     +        proj_name,'?',iproj,'?'
c-----debugging statements (ljm)

      END IF
C
C     Go get a sweeps worth of data
C
 100  CONTINUE

C     Rewind the dorade data file one beam since we
C     had to process one beam to get the beam time.

      IF(ISTAT .EQ. 2) CALL REWNDDOR() 
      NRAYS=0
      TRCKMX=-999.
      TRCKMN= 999.

c-----print *,'DORVOL - Before DORSWP:      iun = ',iun
c-----print *,'DORVOL - Before DORSWP:  irystat = ',irystat
c-----print *,'DORVOL - Before DORSWP: swapping = ',swapping
      write (7,*)'   '
      write (7,*)'DORVOL - Before DORSWP: iun = ',iun
      write (7,*)'DORVOL - Before DORSWP: ifd,ifd_rays=',ifd,ifd_rays
      write (9,*)'   '
      write (9,*)'DORVOL - Before DORSWP: iun = ',iun
      write (9,*)'DORVOL - Before DORSWP: ifd,ifd_rays=',ifd,ifd_rays
      CALL DORSWP(NUFST,JPCK,ELSCAN,ICRTST,CTDBM,CTDBMXH,CTDBMXV,
     X     NTHRSH,TFIELD,TLIMITS,RFNAM,FNUM,P1,P2,P3,P4,P10,AZCOR,
     X     DASANG,ISIDE,REQTIME,ALTMEAN(NSWPS+1),TRCKMEAN(NSWPS+1),
     X     DRFTMEAN(NSWPS+1),PTCHMEAN(NSWPS+1),ROLLMEAN(NSWPS+1),
     X     TRCKMN,TRCKMX,ALTMN,ALTMX,ROTBEG,ROTEND,SPACMN,SPACMX,
     X     SPACMEAN,NRAYS,ISTAT,FLDDAT,RADNAM,FLTNUM,FLDNAM,
     X     IFRST,NFLINP,TILT,ALAT1,ALON1,RADAR_TYPE,NSWPS,IPTR,
     X     THEMODE,DIR,VNYDAT,IFD,IFD_RAYS,JDAY)
C
C     IRYSTAT =  0 --> Normal (good) ray information block (RYIB)
C     IRYSTAT =  1 --> Transition ray information block (RYIB).
C                      Sometimes this is incorrect in dorade files.
C     IRYSTAT =  2 --> Bad ray information block (RYIB), rare
C     Note:  This variable is set to ryib.status value in dorade.c
C
C     ISTAT =  1 --> An end to a sweep containing several beams of data (SWIB)
C     ISTAT =  2 --> Beginning of a new volume (VOLD)
C     ISTAT = >2 --> A "read error" of any kind such as END OF DATA MEDIA, 
C                    CAN'T FIND RADAR, ETC.
C     ISTAT = -2 --> End of a volume scan (VOLD)
C     ISTAT = -3 --> An end of a sweep from a sweep file (NULL)
C
C     Set JSTAT according to ISTAT:
C        JSTAT = 1  --> End of current sweep
C        JSTAT = 2  --> End of current volume
C        JSTAT = 3  --> EOD or I/O ERROR
C
      WRITE(7,*)'DORVOL: AFTER CALL DORSWP'
c-----print *,'DORVOL - After DORSWP:      iun = ',iun
c-----print *,'DORVOL - After DORSWP:  irystat = ',irystat
c-----print *,'DORVOL - After DORSWP: swapping = ',swapping
      IF(IYR.GT.99)THEN
         IYR=IYR-((IYR/100)*100)
      END IF
      IF(EL.GT.180.0)EL=EL-360.0

      PREVIOUS_JSTAT = JSTAT
      IF(ISTAT .EQ. -2)THEN
         ISTAT = -3
         WRITE(7,*)'DORVOL: CHANGED EOV STATUS TO EOS'
         WRITE(7,*)'DORVOL: ISTAT,JSTAT=',ISTAT,JSTAT
      END IF

      IF(ISTAT .EQ.  1) THEN
         JSTAT = 1
c         NSWPS = NSWPS + 1
         write(7,*)
     +   ' An end to a sweep containing several beams of data (SWIB)'
      ELSE IF(ISTAT .EQ.  2) THEN
         JSTAT = 2
         write(7,*)
     +   'Beginning of a new volume (VOLD)'
      ELSE IF(ISTAT .EQ. -2) THEN
         JSTAT = 2
         NSWPS = NSWPS + 1
         write(7,*)
     +   'End of a volume scan (VOLD)'
      ELSE IF(ISTAT .GT.  2) THEN
         JSTAT = 3
         write(7,*)
     +   'A read error of any kind'
      ELSE IF(ISTAT .EQ. -3) THEN
c         JSTAT = 2
         JSTAT = 3
         END_SWEEP_FILE = 1
         NSWPS = NSWPS + 1
         write(7,*)
     +   'An end of a sweep from a sweep file (NULL)'
      END IF
      write(7,*)'DORVOL - (1) EOS, (2) EOV, (3) EOD or I/O ERROR:',
     +     ' jstat=',jstat

c-----debugging statements (ljm)
c     Note: if id = 0, then idchar = ^@ (the NULL character '\0')
      write(7,*)'DORVOL-after dorswp: iun,ijstat,nswps,ID array=',
     +     iun,istat,jstat,nswps
c      call dmpintgr(id,210)
      do j=1,50
         idchar(j)='????????'
      end do
      if(id(11)*id(12)*id(13).ne.0)then
         write (idchar(1),1772)id(11),id(12),id(13)
      end if
      if(id(14)*id(15)*id(16).ne.0)then
         write (idchar(2),1772)id(14),id(15),id(16)
      end if
      if(id(17)*id(18)*id(19)*id(20).ne.0)then
         write (idchar(3),1773)id(17),id(18),id(19),id(20)
      end if
      if(id(21)*id(22).ne.0)then
         write (idchar(4),1774)id(21),id(22)
      end if
 1772 format(3a2)
 1773 format(4a2)
 1774 format(2a2)
      i=idptr_int
      jbeg=5
      jend=5+nflds-1
c      do j=jbeg,jend
c         if(id(i)*id(i+1).ne.0)then
c            write(idchar(j),1775)id(i),id(i+1)
c 1775       format(2a4)
c         end if
c         i=i+5
c      end do
c      call dmpchar(idchar,50)         
c-----debugging statements (ljm)

      IF(JSTAT .EQ. 3 .AND. ICOMBN .EQ. 0) THEN
         IF (NSWPS.LT.2)  THEN
            WRITE(*,501)NSWPS
            GOTO 2
         END IF
      END IF
C
C     END-OF-DATA, but normal processing.
C
      IF ((JSTAT.EQ. 1 .OR. JSTAT.EQ. 2) .OR. 
     x    (JSTAT.EQ. 3 .AND. ICOMBN .EQ. 2)) THEN
C
C     END OF SWEEP OR END OF VOLUME - Read another sweep if too few rays.
C     Here are differences in the logic.

         IF (NRAYS.LT. MNBEM) THEN
            WRITE(*,*)'+++TOO FEW RAYS IN SWEEP. SWEEP SKIPPED. NRAYS=',
     X           NRAYS
            GOTO 100
         END IF
         
         IF(ICOPLANE .EQ. 5) THEN
            ID(IPTR)  =NINT(TRCKMEAN(NSWPS+1)*ID(44))
            ID(IPTR+1)=NINT(DRFTMEAN(NSWPS+1)*ID(44))
            ID(IPTR+2)=NRAYS
            write(7,*)'DORVOL: iptr,id(iptr,+1,+2)=',
     +           iptr,id(iptr),id(iptr+1),id(iptr+2)
         ELSE
            IF(ID(IPTR) .LE. PREVIOUS_FIXED_ANGLE) ILSCHK = .TRUE.
            PREVIOUS_FIXED_ANGLE = ID(IPTR)
            ID(IPTR+1)=SIGN(1.0,DIR)
            TOTAL_NUMBER_RAYS = TOTAL_NUMBER_RAYS + NRAYS
            ID(IPTR+2)= TOTAL_NUMBER_RAYS
c           (LJM - 8/17/09)
            write(7,*)'DORVOL: iptr,trckmean,drftmean,nrays=',
     +           iptr,id(iptr),id(iptr+1),id(iptr+2)

C
C     REVERSE DIRECTION OF SCAN FOR RHI SCANS WITHIN 
C     AZIMUTH SECTORS: 000-045, 225-315, and 315-405.
C     
            FXTMP=FLOAT(ID(IPTR))/FLOAT(JRH7)
            IF (FXTMP.GT.360.) FXTMP=FXTMP-360.
            
            IF (ICOPLANE.EQ.4 .AND. 
     X        ((FXTMP.GE.000. .AND. FXTMP.LT.045.) .OR. 
     X         (FXTMP.GE.225. .AND. FXTMP.LT.315.) .OR. 
     X         (FXTMP.GE.315. .AND. FXTMP.LT.405.))) THEN
               ID(IPTR+1)=-ID(IPTR+1)
            END IF
         END IF
         
         IF(NSWPS .EQ. 0) THEN
            ORLAT = ALAT1 * 10000
            ORLON = ALON1 * 10000
         ENDIF

c         NSWPS=NSWPS+1
c         write(7,*)'DORVOL: #2 increment nswps=',nswps
         IF (NSWPS.GT.MAXEL) THEN
            WRITE(*,125)MAXEL
 125        FORMAT(/,5X,'+++ MAX. NUMBER OF SWEEPS IS ',I3,' +++')
            JSTAT=2
            GO TO 130
         END IF
         VNYQUIST(NSWPS)=VNYDAT
         
         IF(ICOPLANE .EQ. 5) THEN
            ALATS(NSWPS)=ALAT1
            ALONS(NSWPS)=ALON1
            TILTS(NSWPS)=TILT
         ENDIF

         IPTR=IPTR+3
C
C        READ IN NEXT SWEEP
C
         IF (JSTAT.EQ.1) THEN
            GO TO 100
         ENDIF
         IF (JSTAT.EQ.2 .AND. ICOMBN.EQ.2) THEN
c-----debugging statements (ljm)
            write(7,*)'Read another sweep in runover mode'
c-----debugging statements (ljm)
            GO TO 100
         END IF
      END IF

 130  CONTINUE

      IF (JSTAT.EQ.2 .AND. (ICOMBN.EQ.1 .OR. ICOMBN.EQ.2)) THEN
C
C     COMBINE NEXT VOLUME IN SAME FILE WITH CURRENT VOLUME
C     Here are differences in the logic.

         IF (ISTAT .EQ. 2) THEN
            GOTO 100
         ELSEIF (ISTAT .EQ. -2) THEN
            GOTO 50
         END IF

      ELSE IF (JSTAT.EQ. 3 .AND. ICOMBN .EQ. 2) THEN
C
C     TRY TO OPEN A NEW FILE AND COMBINE VOLUMES FROM THERE W/ CURRENT
C
         write(7,*)'DORVOL: EOD and RUNOVER - read another input'
         READ 503,KRD
 503     FORMAT (10A8)
         READ (KRD,504)KOMM
 504     FORMAT (A3)
C
C     If in RUNOVER mode with sweepfiles, INPUTs must be terminated with
C     QUIT or END command.  If QUIT is encountered, then main program
C     will do no more processing.
C
         IF (KOMM .EQ. 'END' .OR. KOMM .EQ. 'QUI') THEN
            IF (KOMM .EQ. 'END') NUFST = 2
            IF (KOMM .EQ. 'QUI') THEN
               write(7,*)"QUIT encountered"
               NUFST = 4
            ENDIF
            IROV = 0
            ICOMBN = 0
            PRINT 505
 505        FORMAT (5X,'+++ End RUNOVER or APPEND mode -- ',
     X           'NO MORE INPUTS WILL BE COMBINED +++')
            IF (NSWPS.LT.2 .AND. END_SWEEP_FILE .EQ. 0) THEN
               WRITE(*,501)NSWPS
 501           FORMAT(5X,'*** VOLUME DISCARDED - ONLY ',I1,
     X              ' ELEVATION SCANS')
               GOTO 2
            END IF
C     
C     VOLUME IS FINISHED
C     
            IFLG=9
            CALL WRRYDK(KPCK,KOUT,INST,LTMP,IFLG,0)
            ID(35)=NSWPS
            ID(36)=NRAYS
            REWIND LTMP
            IELCHK=IELCHK.OR.ILSCHK
            IF(ICOPLANE .EQ. 5) THEN
               CALL AIRCHK(ALTMEAN,TRCKMEAN,DRFTMEAN,PTCHMEAN,
     X                     ROLLMEAN,TILTS,TILTFLG)
            END IF
            CALL SETVOL
c-----debugging statements (ljm)
c            write(7,*)'Dorvol: after airchk/setvol',end_sweep_file,
c     +           nufst
c-----debugging statements (ljm)
            IF(END_SWEEP_FILE .EQ. 1) THEN
c--why is nufst being reset to 3?
c               NUFST = 3
               THE_TILT = 0.0
               IF(ICOPLANE .EQ. 5) THEN
                  IF(TILTFLG .EQ. 1)THEN
                     call get_mode(THEMODE,MODEVALUE)
                     THE_TILT = MODEVALUE/10.
                     IF(TILT .LT. 0.0) THE_TILT = -THE_TILT
                  ENDIF
               END IF
c-----debugging statements (ljm)
               write(7,*)'Dorvol-end-volume: nufst,nswps=',nufst,nswps
c-----debugging statements (ljm)
            END IF
            RETURN
         ELSE
            IF (KOMM .NE. 'INP')THEN
               PRINT 507
 507           FORMAT(5X,'+++ ERROR --- ',
     X              'DO NOT UNDERSTAND WHAT TO DO +++ ',/,
     X              5X,'+++ RUNOVER or APPEND mode must be ',
     X              'terminated with END or QUIT +++')
               STOP
            END IF
            IJNK=1
            CALL INPFIL(KRD,INPTST,IJNK,AZCOR,USER_DEF_ORIGIN,
     X           RADAR_CHOSEN,IROV)
            NUFST=0
            GOTO 50
         ENDIF
         
      ELSE IF((JSTAT.EQ.2 .OR. JSTAT.EQ.3) .AND. ICOMBN .EQ. 0) THEN
C     
C     END OF VOLUME ALSO
C     
 300     CONTINUE
c         write(7,*)"in 300 continue"
         IF (NSWPS.LT.2)  THEN
            WRITE(*,501)NSWPS
            GOTO 2
         END IF
         
C     Get the mode or mean tilt value if requested.
         THE_TILT = 0.0
         IF(ICOPLANE .EQ. 5) THEN
            IF(TILTFLG .EQ. 1)THEN
               call get_mode(THEMODE,MODEVALUE)
               THE_TILT = MODEVALUE/10.
               IF(TILT .LT. 0.0) THE_TILT = -THE_TILT
            ENDIF
         END IF
C     
C     VOLUME IS FINISHED
C     
         IFLG=9
         CALL WRRYDK(KPCK,KOUT,INST,LTMP,IFLG,0)
         ID(35)=NSWPS
         ID(36)=NRAYS
         REWIND LTMP
         IELCHK=IELCHK.OR.ILSCHK
         IF(ICOPLANE .EQ. 5) THEN
            CALL GENAZM(JPCK,IELCHK,MNBEM,FXSTOL,NAST,
     X           ICOPLANE,BASANG)
         ELSE
            CALL GENAZM(JPCK,IELCHK,MNBEM,ELTOL,NAST,
     X           ICOPLANE,BASANG)
         ENDIF
         IF (NAST.NE.0) THEN
            PRINT *,
     X           ' +++  AZIM STRUCTURE CANNOT BE INTERPOLATED  +++'
            JSTAT = 3
            NUFST = 3
            KOMM='???'
            PRINT *,
     X           ' +++  VOLUME DISCARDED - RETURN FROM DORVOL ',
     X           'WITH JSTAT,NUFST=',JSTAT,NUFST,'  +++'
            RETURN
         END IF
         IF(ICOPLANE .EQ. 5) THEN
c-----debugging statements (ljm)
c            write(7,*)'Dorvol-looking good, call airchk: jstat,icombn=',
c     +           jstat,icombn
c-----debugging statements (ljm)
            CALL AIRCHK(ALTMEAN,TRCKMEAN,DRFTMEAN,PTCHMEAN,ROLLMEAN,
     X           TILTS,TILTFLG)
         END IF
         CALL SETVOL
C
C     IROV  =  (-1) APPEND, (0) NORMAL PROCESSING, (1) RUNOVER
C     ICOMBN = (+1) APPEND, (0) NORMAL PROCESSING, (2) RUNOVER
C
C     NUFST
C     =0    NO END YET (NORMAL EXECUTION)
C     =1    PAST ENDING TIME
C     =2    END OF TAPE
C     =3    UNABLE TO INTERPOLATE VOLUME SCAN
C     =4    END OF SWEEP FILES, BUT MAY HAVE ALREADY READ QUIT COMMAND.
C
C     JSTAT = 1  --> End of current sweep
C     JSTAT = 2  --> End of current volume
C     JSTAT = 3  --> EOD or I/O ERROR
C
c-----debugging statements (ljm)
c         write(7,*)'Dorvol-before return: jstat,icombn,nufst=',
c     +        jstat,icombn,nufst
c-----debugging statements (ljm)
         WRITE(*,39)
 39      FORMAT(/)
         IF (JSTAT.EQ.3)THEN
            NUFST=2
            PRINT *,
     X           ' +++  END OF DATA - RETURN FROM DORVOL ',
     X           'WITH JSTAT,NUFST=',JSTAT,NUFST,'  +++'
         ELSEIF (JSTAT.EQ.2)THEN
            IF(ICOMBN.EQ.1)THEN
               NUFST=4
            ELSE
               NUFST=0
            END IF
            PRINT *,
     X           ' +++  END OF CURRENT VOLUME - RETURN FROM DORVOL ',
     X           'WITH JSTAT,NUFST=',JSTAT,NUFST,'  +++'
         ELSEIF (JSTAT.EQ.1)THEN
            NUFST=0
            PRINT *,
     X           ' +++  END OF CURRENT SWEEP - RETURN FROM DORVOL ',
     X           'WITH JSTAT,NUFST=',JSTAT,NUFST,'  +++'
         END IF
         IROV = 0
         ICOMBN = 0
         KOMM='???'
         RETURN
      ELSE
         
         WRITE(*,40)
 40      FORMAT(/,5X,'+++ I/O ERROR READING DORADE VOLUME. CONTACT',
     X        ' SOFTWARE ENGINEER+++')
         STOP
      END IF
      
      END

      
