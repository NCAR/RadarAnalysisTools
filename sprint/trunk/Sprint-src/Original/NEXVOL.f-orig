      SUBROUTINE NEXVOL(NUFST,JPCK,ELSCAN,ICRTST,CTDBM,CTDBMXH,CTDBMXV,
     X     NTHRSH,TFIELD,TLIMITS,RFNAM,FNUM,P1,P2,P3,P4,P10,AZCOR,
     X     DASANG,ISIDE,BEGINNING,USER_DEF_ORIGIN,RADAR_CHOSEN,
     X     VNYQUIST,IFD,IFD_RAYS,IVOL)
C     
C     THIS SUBROUTINE READS ONE VOLUME SCAN IN NEXRAD LEVEL 2 FORMAT
C     AND OUTPUTS IT TO DISK (WRRYDK) IN A SIMPLIFIED FORMAT.
C-------------------------------------------------------------------
C     NFLDDAT - Number of input fields in the current elevation
C               scan.  This is read from the data source.
C     FLDNAM  - Field names within the NEXRAD data source file.
C               Field name array filled in NEXRAD_RDBEAM.
C-------------------------------------------------------------------
C     NFLDS   - Number of fields to be processed as requested 
C               by the user with the INTERP command.
C     IFIELD  - Field names to be processed as requested by the
C               user with the INTERP command. See INTERP.f
C               User-requested fields could include AZ, EL, and TIME
C               which are not NEXRAD fields, but can be generated 
C               from the beam housekeeping.
C-------------------------------------------------------------------
C     NREQFLDS- Number of user-requested fields that could be in
C               the input data set.  NREQFLDS .LE. NFLDS
C     REQ_FLDS- Does not include all user-requested fields (IFIELD),
C               only those names that could be in the NEXRAD dataset.
C-------------------------------------------------------------------
C     NRAYS   - Beam counter inside NEXRAD_RDBEAM.  Will generally be 
C               the same as number of beams read, except NEXVOL has
C               already read one beam with ISTAT = 3 before NEXSWP
C               was called.
C-------------------------------------------------------------------
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
C     ANGXAX = ANGLE OF X AXIS FROM TRUE NORTH (USUALLY 90.0)
C     BASANG = ANGLE OF Y AXIS FROM TRUE NORTH; USED IN COPLANE SCANS
C     DASANG = ANGLE OF Y AXIS FROM BASELINE OF RADARS; USED IN COPLANE SCANS
C        Values of BASANG and DASANG:
C           - Not used for non-coplane scans
C           - Same value when interpolating from coplane scans to coplane
C           - May be different when interpolating from coplane scans to xyz
C     
C     Note: Nyquist velocity and radar constant are saved in SETVOL
C           with and through COMMON /IDBLK/ ID(NID) with 
C           EQUIVALENCE (ID(41),KNQ), (ID(42),KRC) 
C           Also Nyquist velocity was stored in COMMON /SCNDAT/ CFAC
C
C     Note: KDAY - user requested day to be processed (set in PROFIL)
C           JDAY - day of the first ray within the current volume scan
C           IDAY - day of the current ray of data of the current volume scan
C           If the current day changes from the 1st ray day, add 24 to hours.
C     
      INCLUDE 'SPRINT.INC'
c      PARAMETER (MAXEL=150,NID=129+3*MAXEL)
c      PARAMETER (NIOB=85000,MAXIN=8500,MAXLEN=MAXIN/4)
c      PARAMETER (MAXRNG=1024,MAXFLD=16)
c      DATA LFTIM,JRH6,JRH7,IBAD /0,64,100,-32768/

c      PARAMETER (MAXSKP=27,MXCNT=500)
      DIMENSION JPCK(1),ELSCAN(1),IOVER(6),ISIDE(MAXFLD)
      DIMENSION TLIMITS(2,MAXFLD),ITHR(MAXFLD+1)
      DIMENSION CTDBM(MXCNT),CTDBMXH(MXCNT),CTDBMXV(MXCNT)
      CHARACTER*8 KRD(10),RFNAM,P10
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
      
      CHARACTER*8 FLDNAM(MXFNEX)
      CHARACTER*8 REQ_FLDS(MXFNEX)
      DIMENSION FLDDAT(MXGNEX,MXFNEX)

      DIMENSION VNYQUIST(MAXEL)

      COMMON /BYTORD/ MBYTE,SWAPPING
      INTEGER SWAPPING
      COMMON /SCRDSK/ LTMP
      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI,ILLE,ILLZ
      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X     ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      COMMON /INITV/ NRNG,RMIN,GATSPAC,IMIN,ISEC
      LOGICAL ILSCHK
      LOGICAL VOL_FOUND

      CHARACTER*3 KOMM
      CHARACTER*8 INP,IOUT,MET,ICRT,IEMPTY,NAMTIM,BLANK
      DATA INP,IOUT,MET,ICRT/'INPUT','OUTPUT','INTERP','GRID'/
      DATA IEMPTY/'-9999'/
      DATA MFTOL/25/
      DATA EPS/0.15/
      DATA BLANK/'        '/
      INTEGER  REQTIME
      INTEGER  IYR,IMON,IDAY,TDAY
      INTEGER  ISWP,NSWPS,TOTAL_NUMBER_RAYS,IVOL
      INTEGER  FNUM
      INTEGER  BEGINNING,DAY_FOUND,TIME_FOUND,CROSSED_MIDNIGHT
      REAL     AZ,EL,ROTANG,FIXED_ANG,NYQUIST,UNAMB_RANGE
      SAVE     DAY_FOUND,TIME_FOUND,CROSSED_MIDNIGHT
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
C     IROV= (-1) APPEND, (0) NORMAL PROCESSING, (1) RUNOVER
C     
      IF (IROV.EQ.-1) THEN
         ICOMBN=1
      ELSE IF (IROV.EQ.1) THEN
         ICOMBN=2
      ELSE IF (IROV.EQ.0) THEN
         ICOMBN=0
      END IF
      TOTAL_NUMBER_RAYS = 0
      
      IF(IPROJ .EQ. ' ' .OR. IPROJ .EQ. 'COPE' ) IPROJ = 'NXRD'
      ILSCHK = .FALSE.
 2    IFRST  = 1
      IDRGCHG= 0
      INRNG  = 0
      IEOF   = 0
      IFLGBAS= 0
      BASANG = ANGXAX-90.0
c      IPTR   = 129
      IPTR   = IPTR_INT
      ELTOL  = ELTUS
      NFLINP = NFLDS
      ISWP   = 0
      IF(BEGINNING .EQ. 0) THEN
         DAY_FOUND = 0
         TIME_FOUND = 0
      END IF

      IF(IFIELD(NFLDS).EQ.NAMTIM) NFLINP=NFLINP-1
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
 200  CONTINUE
C     
C     START READING NEXRAD DATASET - FIND REQUESTED VOLUME
C     
      PRINT *,'NEXVOL (INIT): IVOL=',IVOL

C     INITIALIZE THRESHOLD COUNTERS (Currently not set up for thresholding)
      DO 23 I=1,MAXFLD+1
         ITHR(I)=0
 23   CONTINUE

      LTMP   = 1
      NSWPS  = 0
      ISWP   = 0
      NRAYS  = 0
      IREWND = 0

C     Initialize (IFLG=-9) Unit LTMP=1 (fort.1) as SPRINT's internal disk file
C
      WRITE(LTMP) LTMP
      REWIND LTMP
      CALL WRRYDK(KPCK,KOUT,INST,LTMP,-9,0)
C
C     GET TO RIGHT VOLUME (BY TIME)
C
      IHR  = 0
      IMIN = 0
      ISEC = 0
      REQTIME = KETIM
C
C     Create a new list of fields from those requested, excluding those
C     fields that will be derived from beam housekeeping information.
C
      NREQFLDS = 0
      DO N = 1,NFLDS
         IF(IFIELD(N)(1:2) .NE. 'AZ' .AND.
     X      IFIELD(N)(1:2) .NE. 'EL' .AND.
     X      IFIELD(N)(1:4) .NE. 'TIME')THEN
            NREQFLDS = NREQFLDS + 1
            REQ_FLDS(NREQFLDS) = IFIELD(N)
         END IF
      END DO

 50   CALL NEXRAD_RDBEAM(NUFST,KETIM,IUN,IREWND,ISTAT,IVOL,  
     X     ISWP,IYR, IMON, IDAY, IHR, IMIN, ISEC, MSEC, JULDAY,
     X     AZ, EL, ROTANG, FIXED_ANG, NYQUIST,UNAMB_RANGE,
     X     FLDDAT,FLDNAM,REQ_FLDS,NREQFLDS,NFLDDAT,NRNG,RMIN,GATSPAC, 
     X     NRAYS,SWAPPING,TAPE_UNIT)
C
C     ISTAT=0  --> START OF A NEW SWEEP (ELEVATION SCAN)
C     ISTAT=1  --> INTERMEDIATE BEAM
C     ISTAT=2  --> END OF A SWEEP (ELEVATION SCAN)
C     ISTAT=3  --> BEGINNING OF A NEW VOLUME SCAN
C     ISTAT=4  --> END OF A VOLUME SCAN
C     ISTAT=5  --> STATUS DATA CONTAINS NO BEAM DATA
C     ISTAT=6  --> END OF DATA
C     ISTAT=7  --> READ ERROR
C
c-----debug (ljm)
      write(7,*)'Nexvol: after #50 - a nexrad beam has been read',
     +     ' istat=',istat
c      if(istat.le.3)then
         write(7,*)'Nexvol   ymd,hms= ',iyr,imon,iday,ihr,imin,isec
         write(7,*)'Available nexrad fields: ',
     +        (fldnam(nn),nn=1,nflddat)
         write(7,*)'   All requested fields: ',
     +        (ifield(nn),nn=1,nflds)
         write(7,*)'  Requested data fields: ',
     +        (req_flds(nn),nn=1,nreqflds)
         write(7,*)' Range (min,spac,nr,na): ',
     +        rmin,gatspac,nrng,nrays
c      end if
c-----debug (ljm)

      BEGINNING = 1
      IF(ISTAT .EQ. 5) THEN 
         GOTO 50
      ELSE IF(ISTAT .EQ. 6) THEN
         PRINT *,"END OF DATA FILE ON UNIT ",IUN
         NUFST = 1
         RETURN
      ELSE IF(ISTAT .EQ. 7) THEN
         PRINT *,"DATA READ ERROR ON UNIT ",IUN
         NUFST = 1
      END IF

C
CCheck the day.+++++++++++++++++++++++++++++++++++++++++++++++++
      IF(DAY_FOUND .EQ. 0) THEN
         TDAY = 10000*IYR + 100*IMON + IDAY
         IF(TDAY .GT. KDAY) THEN
            PRINT *,"DATE ON TAPE ",TDAY," IS PAST DATE OF DATA ",KDAY
            NUFST = 1
            RETURN
         ELSE IF(TDAY .LT. KDAY) THEN
            GOTO 50  
         ELSE
            DAY_FOUND = 1
         END IF
      END IF

C
CNOW CHECK THE TIME+++++++++++++++++++++++++++++++++++++++++++++++
      IF(KBTIM .EQ. 0) THEN
         TIME_FOUND = 1
      ELSE
         IFTIM = 10000*IHR + 100*IMIN + ISEC
         IF((IFTIM .GT. KETIM) .AND. (TIME_FOUND .EQ. 0))THEN
            PRINT 233,IUN,IFTIM,KETIM
 233        FORMAT(/1X,'+++  UNIT: ',I2,5X,'INITIAL TIME ON TAPE: ',I6,
     X           '  IS PAST THE REQUESTED ENDING TIME TO PROCESS: ',I6,
     X           '  +++'/)
            NUFST=1
            RETURN
         ELSE IF(IFTIM .LT. KBTIM) THEN
            PRINT *,"SKIPPING VOL: DATE ",TDAY," TIME ",IHR,IMIN,ISEC,MSEC
            CALL NEXRAD_SKIP_VOLUME(ISTAT,SWAPPING)
            IF((ISTAT .EQ. 7) .OR. (ISTAT .EQ. 6)) THEN
               NUFST = 1
               RETURN
            END IF
            GO TO 50
         ELSE 
C     WE HAVE REACHED OUR START TIME.
            TIME_FOUND = 1 
         END IF

      END IF

CEND CHECK TIME +++++++++++++++++++++++++++++++++++++++++++++++++++++++


COUR VOLUME IS FOUND START PROCESSING 

      JDAY = IDAY
      IVOL = IVOL + 1
      VOL_FOUND = .TRUE.
      PRINT *,"VOLUME FOUND  ",KDAY," TIME ",IHR,':',IMIN,':',ISEC
      write(7,*)"VOLUME FOUND  ",KDAY," TIME ",IHR,':',IMIN,':',ISEC
      PRINT *
      PRINT *,"     +++GRIDDING DATA IN R,A,E TO 3-D CARTESIAN+++"
      PRINT 236
 236  FORMAT(//6X,'SCAN',4X,'NYQUIST',14X,'ELEVATION',28X,'AZIMUTH',
     X     13X,'SPACING',16X,'BEAMS'/4X,'NO',3X,'DIR',1X,'VELOCITY',
     X     2X,'FIXED',6X,'MIN',6X,'MAX',5X,'MEAN',10X,'BEG',7X,'END',
     X     7X,'MIN',6X,'MAX',5X,'MEAN',6X,'GOOD',3X,'BAD')  

 65   CONTINUE
        
      ID(4) = IHR
      CALL INITVOL(IPROJ)
C
C     READ IN SWEEPS FOR THIS VOLUME
C
 100  CONTINUE
      NRAYS = 0
      CALL NEXSWP(IVOL,ISWP,ISTAT,NRAYS,IPTR,FLDDAT,IFRST,NTHRSH,
     X     TFIELD,TLIMITS,RFNAM,FNUM,NUFST,NYQUIST,IFD,IFD_RAYS,
     X     VOL_FOUND,JDAY,CNT_ELEV,SUM_ELEV)
C
C     ISTAT=0  --> START OF NEW ELEVATION SCAN
C     ISTAT=1  --> INTERMEDIATE BEAM
C     ISTAT=2  --> END OF AN ELEVATION SCAN
C     ISTAT=3  --> BEGINNING OF A NEW VOLUME SCAN
C     ISTAT=4  --> END OF A VOLUME SCAN
C     ISTAT=5  --> STATUS DATA OR FILE HEADER CONTAINS NO BEAM DATA
C     ISTAT=6  --> END OF PHYSICAL MEDIA EITHER TAPE OR DISK FILE
C     ISTAT=7  --> READ ERROR
C
c-----debug (ljm)
      if(cnt_elev.gt.0.0)then
         avg_elev=sum_elev/cnt_elev
         scl_elev=avg_elev*8.0*4096.0/180.0
         write (7,*)'Nexvol (eos): Elev - cnt,avg,scl=',
     +        cnt_elev,avg_elev,scl_elev
      else
         write (7,*)'Nexvol (eos): cnt_elev is 0'
      end if
      write(7,*)'Nexvol (eos): ivol,iswp,nrays=',ivol,iswp,nrays,
     +     ' istat=',istat
c-----debug (ljm)
      PRINT_HEADING = 0
      IF( ISTAT .EQ. 6) THEN
         PRINT *,"END OF DATA ON ",IUN
         NUFST = 2
         RETURN
      END IF

      IF (ISTAT.EQ.2 .OR. ISTAT.EQ.4)THEN
C
C     END OF SWEEP; PROCESS IT
C
         IF (NRAYS.LT.MNBEM) THEN
            WRITE(*,*)'+++TOO FEW RAYS IN SWEEP. SWEEP SKIPPED. NRAYS=',
     X           NRAYS
            GOTO 100
         END IF
         
         ID(IPTR+1) = 1
         TOTAL_NUMBER_RAYS = TOTAL_NUMBER_RAYS + NRAYS
         ID(IPTR+2) = TOTAL_NUMBER_RAYS
         NSWPS = NSWPS + 1
c--------debug (ljm)
c         write(7,*)'Nexvol (eos): nswps,nrays,istat=',nswps,nrays,istat
c--------debug (ljm)
         IF (NSWPS.GT.MAXEL) THEN
            WRITE(*,125)MAXEL
 125        FORMAT(/,5X,'+++ MAX. NUMBER OF SWEEPS IS ',I3,' +++')
            STOP
         END IF
         VNYQUIST(NSWPS)=NYQUIST
         IF(NSWPS.EQ.1)VNYQ=NYQUIST
         
C     If ISTAT = 1 read the next sweep ELSE
C     If ISTAT = 6 (end of data) and icombine = 1 or 2 then combine 
C                next volume in same file with current volume.  ELSE
C     If ISTAT = 2 or 3 (end of sweep) or (begin new volume) then
         
         IPTR=IPTR+3
         IF(ISTAT .EQ. 2) THEN
            GOTO 100 
         ELSE IF (ISTAT.EQ.4 .AND. (ICOMBN.EQ.1 .OR. ICOMBN.EQ.2)) THEN
C     COMBINE NEXT VOLUME IN SAME FILE WITH CURRENT VOLUME 
            GOTO 65
         ELSE IF (ISTAT.EQ.6 .AND. ICOMBN.EQ.2 ) THEN
C     
C     TRY TO OPEN A NEW FILE AND COMBINE VOLUMES FROM THERE W/ CURRENT
C
            READ 503,KRD
 503        FORMAT (10A8)
            READ (KRD,504)KOMM
 504        FORMAT (A3)
            IF (KOMM.NE.'INP') THEN
               PRINT 505
 505           FORMAT (5X,'***COULD NOT FIND NEXT INPUT CARD.',
     X              ' NO MORE VOLUMES WILL BE COMBINED.')
               IF (NSWPS.LT.2) THEN
                  WRITE(*,501)NSWPS
 501              FORMAT(5X,'*** VOLUME DISCARDED - ONLY ',I1,
     X                 ' ELEVATION SCANS')
                  GOTO 2
               END IF
            
c--------------debug (ljm)
               write(7,*)'Nexvol (comb): nswps,nrays=',nswps,nrays
c--------------debug (ljm)
               CALL WRRYDK(KPCK,KOUT,INST,LTMP,9,0)
               ID(35)=NSWPS
               ID(36)=NRAYS
               REWIND LTMP
               CALL SETVOL
               NUFST=0
               RETURN
            ELSE
               IJNK=1
               CALL INPFIL(KRD,INPTST,IJNK,AZCOR,USER_DEF_ORIGIN,
     X              RADAR_CHOSEN,IROV)
               GOTO 50
            ENDIF
         
         ELSE IF (ISTAT.EQ.4) THEN
C     
C     END OF VOLUME ALSO
C     
            IF (NSWPS.LT.2) THEN
               WRITE(*,501)NSWPS
               GOTO 2
            END IF
         
c-----------debug (ljm)
            write(7,*)
     +           'Nexvol (eov): nswps,nrays,istat=',nswps,nrays,istat
c-----------debug (ljm)
            CALL WRRYDK(KPCK,KOUT,INST,LTMP,9,0)
            ID(35)=NSWPS
            ID(36)=NRAYS
            REWIND LTMP
            CALL SETVOL
            IF(NUFST .NE. 3) NUFST=0
            IF(ISTAT .EQ. 6) NUFST = 2
            RETURN
         END IF
      
      ELSE
         
         WRITE(*,40)
 40      FORMAT(/,5X,'+++ I/O ERROR READING NEXRAD VOLUME. CONTACT',
     X        ' SOFTWARE ENGINEER+++')
         STOP
      END IF 

      RETURN
      END
