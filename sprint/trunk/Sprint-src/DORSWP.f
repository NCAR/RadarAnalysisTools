      SUBROUTINE DORSWP(NUFST,JPCK,ELSCAN,ICRTST,CTDBM,CTDBMXH,CTDBMXV,
     X     NTHRSH,TFIELD,TLIMITS,RFNAM,FNUM,P1,P2,P3,P4,P10,AZCOR,
     X     DASANG,ISIDE,REQTIME,ALTMEAN,TRCKMEAN,DRFTMEAN,PTCHMEAN,
     X     ROLLMEAN,TRCKMN,TRCKMX,ALTMN,ALTMX,ROTBEG,ROTEND,SPACMN,
     X     SPACMX,SPACMEAN,NRAYS,ISTAT,FLDDAT,RADNAM,FLTNUM,FLDNAM,
     X     IFRST,NFLINP,TILT,ALAT1,ALON1,RADAR_TYPE,NSWPS,IPTR,
     X     THEMODE,DIR,VNYDAT,IFD,IFD_RAYS,JDAY)
C
C     READS A SWEEP OF DORADE BEAMS: EITHER FROM SWEEPFILES 
C           OR FROM A MULTI-SWEEP FILE (VOLUME SCAN).
C     Note: Track-relative rotation, azimuth, elevation and tilt 
C     angles are calculated in rdbeam2.c using Dick Oye's routine.
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
C           007   - Hour of current beam
C           008   - Minute of current beam
C           009   - Second of current beam
C           IDPTR - Initialized value is IDPTR_INT
C              +0 - First four characters of requested field name
C              +1 - Last  four characters of requested field name
C              +2 - Radar constant or Nyquist, depending on field
C                   requested
C              +4 - Scale factor (64,100,100,8) for requested azimuth, 
C                   elevation, rotation angle, or time (AZ,EL,ROTANG,TIME) 
C                   field, 100 for any normal data field, or ISCALE for
C                   any of the analytic fields (ANLACT or ANLREP).
C           IDPTR is incremented by 5.
C            IPTR - Initial value is IPTR_INT
C              +0 - Fixed angle of current volume scan
C        Set in INITVOL:
C            31  - See RNGFIL: (RG1) Adjusted range to first gate (m)
C                  RG1 = R0 + RNOTUS = data + user-supplied correction
C                  ID(31)=INT(RG1)
C            32  - ID(32)=NINT((RG1-ID(31))*1000.0)
C            33  - Range gate spacing (m)
C            37  - First data word address (10) in KOUT
C            47  -  100*X-coordinate for the radar
C            48  -  100*Y-coordinate for the radar
C            46  - 1000*Z-coordinate for the radar
C        Set in DORVOL:
C            35  - Sweep counter
C            36  - Number of rays (beams)
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
C     NFLDDAT - Number of fields in the present elevation scan.
C               this is read off the data source.
C     NFLDS   - Number of fields to be processed as requested 
C               by the user with the INTERP command.
C     IFIELD  - Field names to be processed as requested by the
C               user with the INTERP command. See INTERP.f
C     REQ_FLDS- Field names that could be in the NEXRAD dataset.
C               IFIELD lmight include AZ, EL, and TIME which are
C               not DORADE fields, but can be generated from the
C               beam housekeeping.
C     FLDNAM  - Field names within the DORADE data source file.
C               Field name array filled in RDBEAM.
C     NRAYS   - Beam counter for the number of beams read by RDBEAM.
C     GDBEAMS - Beam counter incremented here, GDBEAMS .le. NRAYS.
C     BDBEAMS - Number of beams that have been thrown out.
C     TOTBEAMS- Number of beams that have been read.
C
      INCLUDE 'SPRINT.INC'
c      PARAMETER (MAXSKP=27,MXCNT=500)
      COMMON /IO/KPCK(NIOB),KOUT(MAXIN),IBUF(MAXIN),NBUF(MAXLEN,4),
     X     IZ8(17),ILSTREC
      COMMON /IDBLK/ID(NID)

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
      
      COMMON /INITV/ NRNG,RMIN,GATSPAC,INIT_MIN,INIT_SEC
      COMMON /CRNG/ELTUS,MNBEM,RUSR1,RUSR2,RG1,RG2,DRG,NRG,VNYQ,
     X     RNOTUS,DRGUS,CFAC1,CFAC2,CFAC3
      COMMON /CRNGC/ IRCFXL
      CHARACTER*8 IRCFXL
      
      COMMON /CPRO/KDAY,KBTIM,KETIM,IROV,ISWMTH,FXSTOL
      COMMON /CPROC/ IREORD(3)
      CHARACTER*1 IREORD
      
      COMMON /BYTORD/ MBYTE,SWAPPING
      COMMON /SCRDSK/ LTMP
      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI,ILLE,ILLZ
      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X     ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      INTEGER RADAR_TYPE,SWAPPING
      REAL    ELMIN,ELMAX,ELMEAN,AZMAX,AZMIN,AZTMP
      REAL    MINSPACE,MAXSPACE
      REAL    FIXED_ANGLE,PREVIOUS_FIXED,ELTOL
      REAL    BEGAZ
      REAL    ALAT1,ALON1,FDATA
      REAL    RADCON
      REAL    DIF,DIR
      INTEGER NSWPS,SKIP_SWEEP,NRAYS
      INTEGER THEMODE(1000),MODEINDEX
      INTEGER FNUM
      INTEGER BDBEAMS,GDBEAMS,TOTBEAMS
      SAVE PREVIOUS_FIXED
      DATA DTR /0.0174533/

      CHARACTER*8 RADNAM,FLTNUM,FLDNAM(MXFDOR),REQ_FLDS(MAXFLD)
      DIMENSION FLDDAT(MXGDOR,MXFDOR)

      CHARACTER*4 PROJ_NAME
      SAVE REQ_FLDS
      CHARACTER*8 TFIELD(2,MAXFLD),CTEMP,RFNAM,P10
      DIMENSION NUMTH(MAXFLD),MTFIEL(MAXFLD)
      COMMON /AIRBRN/ ALATS(MAXEL),ALONS(MAXEL),IALTFLG,THE_TILT

      ANGXAXT=90.
      BASANG=ANGXAX-90.0
      ELTOL = AMIN1(ELTUS,FXSTOL)
c-----debug (ljm)
      write(6,*)'   DORSWP: start reading a sweep of data from unit=',
     +     iun
      write(6,*)'           eltus,fxstol,eltol,angxax=',
     +     eltus,fxstol,eltol,angxax
      write(6,*)'           jrh6, jrh7,iptr,mxfdor=',
     +     jrh6,jrh7,iptr,mxfdor
c-----debug (ljm)

      BDBEAMS = 0
      GDBEAMS = 0
      TOTBEAMS= 0
c      IDPTR  = 76
      IDPTR  = IDPTR_INT
      IFC    = 0
      IREWND = 0
      DIR    = 0.0
      HEADMX = 0.0
      HEADMN = 1000.0
      ALTMX  = 0.0
      ALTMN  = 0.0
      TRCKSM = 0.0
      ALTSUM = 0.0
      DRFTSM = 0.0
      PTCHSM = 0.0
      ROLLSM = 0.0
      ROTSUM = 0.0
      SPACMX = 0.0
      SPACMN = 1000.
      SPACSM = 0.0
      ROTKP  =-1000.
      ELMIN  = 1000.
      ELMAX  = 0.0
      ELMEAN = 0.0
      MINSPACE = 0.0
      MAXSPACE = 0.0
      AZMIN  = 1000.0
      AZMAX  = 0.0
      ALATSM = 0.0
      ALONSM = 0.0
      TILTSM = 0.0
      TILTMX = 0.0
      TILTMN = 1000.
      PREVIOUS_FIXED = 0.0
      SKIP_SWEEP = 0

C     IBEGRG - some goofy number related to range to first gate in meters
C
      IBEGRG=ID(31)*1000 + ID(32)
      if(ifrst.eq.1)then
         write(7,*)'   DORSWP: id(31),id(32),ibegrg=',id(31),id(32),ibegrg
      endif

      DO J=1,MXFDOR
         DO I=1,MXGDOR
            FLDDAT(I,J)= -32768
         END DO
      END DO
 
      DO J = 1,NFLDS
         REQ_FLDS(J) = IFIELD(J)
         CALL USRFLDS(IFIELD(J),J)
      END DO
C     
C     CALCULATE RADAR COORDINATES IN ROTATED (IF ROTATED) COORD. SYS.
C     
          IF (ANGXAX.NE.90.0) THEN
             DHETA=(ANGXAX-90.0)*DTR
             XORR=FLOAT(ID(47))/100.*COS(DHETA) - FLOAT(ID(48))/100.
     X            *SIN(DHETA)
             YORR=FLOAT(ID(47))/100.*SIN(DHETA) + FLOAT(ID(48))/100.
     X            *COS(DHETA)
             ZORR=FLOAT(ID(46))/1000.
          ELSE
             XORR=FLOAT(ID(47))/100.
             YORR=FLOAT(ID(48))/100.
             ZORR=FLOAT(ID(46))/1000.
          END IF

C     
C     LINE 100 IS THE START OF THE LOOP FOR READING A SWEEP'S WORTH OF BEAMS
C
 100  CALL RDBEAM(IUN, IREWND, ISTAT, IVOL, IYR,IMON,IDAY, 
     X     IHR, IMIN, ISEC, MSEC, NUMRADS, ITP,NFLDDAT,
     X     NUMFREQ, NUMIPP, NRNG,ISWP,JULDAY,IRYSTAT,
     X     RADAR_TYPE,REQTIME,RADCON, SCANRATE, ALON, ALAT,
     X     VNYDAT,RNGMX,RMIN, GATSPAC,AZ,EL,ALTMSL,PRESALT,
     X     ALTGND,GRNDSPDEW,GNDSPDNS,VERVEL,HEADING,ROLL,
     X     PITCH,DRIFT,ROTANG,TILT,UAIR,VAIR,WAIR,
     X     HEDCHGRT,PITCHGRT,FLDDAT,BAD,FXANG,RADNAM,
     X     FLDNAM,PROJ_NAME,FLTNUM,SWAPPING)
C
C     ISTAT =  0 ---> A successful read of a beams worth of data (many fields)
C     ISTAT =  1 ---> An end to a sweep containing several beams of data (SWIB)
C     ISTAT =  2 ---> Beginning of a new volume (VOLD)
C     ISTAT = >2 ---> A "read error" of any kind such as END OF DATA MEDIA, CAN'T
C                     FIND RADAR, ETC.
C     ISTAT = -2 ---> End of a volume scan (VOLD)
C     ISTAT = -3 ---> An end of a sweep from a sweep file (NULL)(RKTB)
C
C     RMIN    - range to first gate in meters
C     RNGMX   - range to last gate in kilometers (not set in rdbeam)
C     GATSPAC - range gate spacing in meters
C     NRNG    - number of range gates
C
      TOTBEAMS = TOTBEAMS + 1
      write(7,*)'DORSWP: istat,irystat=',istat,irystat
      write(6,1767)'DORSWP#1:',totbeams,itp,fxang,az,el,dir,icoplane
 1767 format(A9,' beams,itp,fx,az,el,dir,icoplane=',2i8,4f10.3,i8)

      if(ifrst.eq.1)then
         write(7,*)'   DORSWP: frst,ptr,totb,fae=',
     +        ifrst,iptr,totbeams,fxang,az,el
      end if
      IF(RMIN.LT.0.0)RMIN=0.0
      IF(IYR.GT.99)THEN
         IYR=IYR-((IYR/100)*100)
      END IF
      IF(EL.GT.180.0)EL=EL-360.0

C     See DUMP for turning on dumping of every IFD_RAYS housekeeping.
C
      IF(IFD.EQ.1)THEN
         IGSP=NINT(GATSPAC)
         IRN=RMIN/1000.
         IRX=(RMIN+(NRNG-1)*GATSPAC)/1000.
         IDATE=IYR*10000+IMON*100+IDAY
         ITIME=IHR*10000+IMIN*100+ISEC
      END IF
      
      IF(ISTAT .NE. 0) THEN
         GOTO 200
      END IF
      
C     A BAD RYIB DESCRIPTOR WAS FOUND
C     
      IF(IRYSTAT .NE. 0) THEN
         BDBEAMS=BDBEAMS+1
         print *,"BAD BEAM IN DORSWP AT ",IHR,IMIN,ISEC,MSEC,BDBEAMS
         GOTO 100
      ENDIF
      
 150  CONTINUE

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
c      write(6,1767)'DORSWP#2:',totbeams,itp,fxang,az,el,dir,icoplane

      IF (ICOPLANE.EQ.4 .AND. ANGXAX.NE.90.0) THEN

C     GROUND-BASED RHI
C
C     DUE TO CONTORTED WAY THAT RHIS HAVE TO BE INTERPOLATED,
C     X-AXIS ROTATIONS NEED TO BE HANDLED VIA AZIMUTH CORRECTION
         
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

C     Time for the beam
C     Increase the hour by 24 to allow for crossing 00 hour within a volume scan;
C     otherwise, the TIME field will be incorrect.  Only one day-change allowed.
C           JDAY - day of the first ray within the current volume scan
C           IDAY - day of current ray within the current volume scan
C
      KOUT(3) = IHR
      IF (IDAY.NE.JDAY)THEN
         IHR = IHR + 24
         KOUT(3)=KOUT(3)+24
      END IF
c      write(7,*)'DORSWP: jday,imon,iday,hms=',
c     x     jday,imon,iday,ihr,imin,isec
      KOUT(4) = IMIN
      KOUT(5) = ISEC
      ID(7)   = KOUT(3)
      ID(8)   = KOUT(4)
      ID(9)   = KOUT(5)

C     Scaling factors

      KOUT(6) = JRH6
      KOUT(7) = JRH7

C     Number of range gates (NRG = NRNG)

      KOUT(8) = NRNG
      IF(NRNG .GT. MXGDOR) THEN
         PRINT *,"THE NUMBER OF RANGE GATES ",NRNG
         PRINT *,"THE MAX ALLOWED BY SPRINT IS ",MXGDOR
         stop
      END IF
      KST     = ID(37)
c      write(7,*)'DORSWP: kst=',kst

C     GROUND BASED RADAR - Azimuth and Elevation angles
C     Interchange the roles of AZ and EL when it is an RHI scan.
C
      AZINP = AZ
      ELINP = EL
      AZC   = AZ
      IF(ITP .EQ. 3)THEN
         AZ = ELINP
         EL = AZINP
         AZC = 90.0 - ELINP
      END IF
c------temporarily comment out
c      write(7,*)'DORSWP: itp,icop,ae-in,az,el,azc=',itp,icoplane,
c     +     azinp,elinp,az,el,azc
c      write(6,1767)'DORSWP#3:',totbeams,itp,fxang,az,el,dir,icoplane

      IF (ICOPLANE .EQ. 0 .OR. ICOPLANE .EQ. 4) THEN
          KOUT(1) = NINT((AZINP + AZCOR) * JRH6)
          KOUT(2) = NINT(ELINP * JRH7)
          IF(EL .LT. ELMIN) ELMIN = EL
          IF(EL .GT. ELMAX) ELMAX = EL
          IF(GATSPAC .LT. MINSPACE) MINSPACE = GATSPAC
          IF(GATSPAC .GT. MAXSPACE) MAXSPACE = GATSPAC
          ELMEAN = EL + ELMEAN
          IF(NRAYS .EQ. 0) THEN
             PREVIOUS_FIXED = FIXED_ANGLE
             FIXED_ANGLE = FXANG
             BEGAZ = AZ
             PAZ   = AZ
             GDBEAMS = 0
          END IF

C     Check the elevation (PPI) or azimuth (RHI) angle tolerance.
C        AZINP - Current azimuth angle (degrees)
C        ELINP - Current elevation angle (degrees)
C        If RHI, interchange AZ and EL
C           AZ = ELINP (Current elevation angle)
C           EL = AZINP (Current elevation angle)
C        FXANG - Fixed angle (degrees)
          
          IF (ICOPLANE.EQ.4) THEN
C
C     Constant azimuth scan (RHI) : interpolation to cartesian (x,y,z)
C     AZC - equivalent azimuth angle in vertical scan plane.  Elevation
C     is measured counter-clockwise whereas AZC is measured clockwise.  
C     Note: These "negative azimuth" angle beams must be discarded in 
C           order for the interpolation to work.  Negative azimuths are
C           not allowed in the TRPVOL interpolation code.
C
             AZC=90.0 - ELINP
             IF (AZC.LT.0.05 .OR. AZC.GT.89.95) THEN
C**LJM           WRITE(*,*)'***NEG. ELEV. ANGLE ---BEAM DISCARDED***'
                BDBEAMS=BDBEAMS+1
                GOTO 100
             END IF
          END IF

          IF (ABS((FXANG-EL)).GT.ELTOL) THEN
             BDBEAMS = BDBEAMS + 1
             IF(IFD.EQ.1)THEN
c                IF(MOD(TOTBEAMS,IFD_RAYS).EQ.0)THEN
                   WRITE(7,773)IDATE,ITIME,AZINP,ELINP,FXANG,NRNG,IGSP,
     +                  RADAR_TYPE,ICOPLANE,IVOL,ISWP,NFLDDAT,ITP,
     +                  BDBEAMS,GDBEAMS,ISTAT,IRYSTAT
 773               FORMAT(1X,'D=',I6.6,' T=',I6.6,' A=',F5.1,' E=',F5.1,
     +                  ' F=',F5.1,' Ng=',I4,' Gs=',I4,
     +                  ' Rtyp=',I2,' Cpl=',I2,
     +                  ' Vl=',I3,' Sw=',I4,' Nf=',I2,' Md=',I1,
     +                  ' Nb=',I4,' Na=',I4,' St=',2I2,' Bad')
c                END IF
             END IF
             GOTO 100
          END IF
          AZTMP = AZ
          DIF=AZTMP - PAZ  
          PAZ = AZTMP   
          IF (ABS(DIF).GT.180) DIF=DIF-SIGN(360.0,DIF)
          ABDIF=ABS(DIF)
          IF (ABDIF.GT.AZMAX) AZMAX=ABDIF
          IF (ABDIF.LT.AZMIN .AND. ABDIF.GT.0.0) AZMIN=ABDIF   
          DIR=DIR+DIF 
          NRAYS=NRAYS+1
c          write(7,*)'   DORSWP: before nrays check, nrays,iptr,fx=',
c     +         nrays,iptr,fxang
          IF(NRAYS .EQ. 1) THEN
             ID(IPTR) = NINT(FIXED_ANGLE * JRH7)
             fixang=float(id(iptr))/float(jrh7)
             write(7,*)'   DORSWP - 1st good beam: fxang=',fixang
          END IF
          GDBEAMS=GDBEAMS+1
          IF(IFD.EQ.1)THEN
             IF(MOD(TOTBEAMS,IFD_RAYS).EQ.0)THEN
                WRITE(7,774)IDATE,ITIME,AZINP,ELINP,FXANG,NRNG,
     +               IGSP,RADAR_TYPE,ICOPLANE,IVOL,ISWP,NFLDDAT,
     +               ITP,BDBEAMS,GDBEAMS,ISTAT,IRYSTAT
 774               FORMAT(1X,'D=',I6.6,' T=',I6.6,' A=',F5.1,' E=',F5.1,
     +                  ' F=',F5.1,' Ng=',I4,' Gs=',I4,
     +                  ' Rtyp=',I2,' Cpl=',I2,
     +                  ' Vl=',I3,' Sw=',I4,' Nf=',I2,' Md=',I1,
     +                  ' Nb=',I4,' Na=',I4,' St=',2I2)
             END IF
          END IF
      END IF

C     AIRBORNE - FOR, AFT, OR TAIL
C
      IF (ICOPLANE.EQ.5) THEN
         write(7,*)'DORSWP - airborne: icoplane=',icoplane
         IF (ROTKP.NE.-1000.) THEN
            SPAC=ROTANG-ROTKP
            IF (SPAC.LT.0.0) SPAC=(ROTANG+360.-ROTKP)
            IF (SPAC.GT.180.0) SPAC=360.0-SPAC
            SPACSM=SPACSM+SPAC
            IF (SPAC.GT.SPACMX) SPACMX=SPAC
            IF (SPAC.LT.SPACMN) SPACMN=SPAC
         END IF
         ROTKP=ROTANG
         IF (ROTANG.GT.360.0) ROTANG=ROTANG-360.0
         IF (ROTANG.LT.0.0)   ROTANG=ROTANG+360.0
C
C     CALCULATE THE TRACK-RELATIVE ROTATION AND TILT ANGLES
C        Note: Now calculated inside rdbeam2.c
C
         KOUT(1)= NINT((ROTANG + AZCOR) * JRH6)
         ROT=ROTANG + AZCOR
         KOUT(2)=HEADING*JRH7
         KOUT(6)=NINT(TILT*64.)
         MODEINDEX = NINT(TILT * 10)
         IF(MODEINDEX .LT. 0) MODEINDEX = - MODEINDEX
         THEMODE(MODEINDEX) = THEMODE(MODEINDEX) + 1
         KOUT(9) =NINT(ALAT*10000.)
         KOUT(10)=NINT(ALON*10000.)
         TRACK = HEADING+DRIFT

        IF (NRAYS.EQ.0) THEN
            ALAT1=ALAT
            ALON1=ALON             
            ROTBEG=ROTANG
            TILTMX=TILT
            TILTMN=TILT
         END IF
         IF (TRACK.GT.TRCKMX)   TRCKMX=TRACK
         IF (TRACK.LT.TRCKMN)   TRCKMN=TRACK
         IF (TILT .GT.TILTMX)   TILTMX=TILT
         IF (TILT .LT.TILTMN)   TILTMN=TILT
         IF (PRESALT .GT.ALTMX ) ALTMX=PRESALT
         IF (PRESALT .LT.ALTMN ) ALTMN=PRESALT

         ALTSUM=ALTSUM+PRESALT
         TRCKSM=TRCKSM+TRACK
         DRFTSM=DRFTSM+DRIFT
         PTCHSM=PTCHSM+PITCH
         ROLLSM=ROLLSM+ROLL
         ALATSM=ALATSM+ALAT
         ALONSM=ALONSM+ALON
         TILTSM=TILTSM+TILT

c
c     Calculate current aircraft (xj,y0) relative to first beam
c     (lat,lon) position using current (lat,lon). y0 is used in
c     ANLACT.
c         
         angx = 90.0 + track
         if (angx.gt.360.0) angx=angx-360.0
         if (alats(1).eq.0.0) then
            orlat1=alat1
            orlon1=-alon1
            alon2=-alon
C            call ll2xydrv(alat,alon2,xj,y0,orlat1,orlon1,288.4)
            call ll2xydrv(alat,alon2,xj,y0,orlat1,orlon1,angx)
         else
            orlat1=alats(1)
            orlon1=-alons(1)
            alon2=-alon
C            call ll2xydrv(alat,alon2,xj,y0,orlat1,orlon1,288.4)
             call ll2xydrv(alat,alon2,xj,y0,orlat1,orlon1,angx)
         end if
         NRAYS=NRAYS+1
         GDBEAMS=GDBEAMS+1
      END IF
C
C     THE END OF IF COPLANE .EQ. 5
C         
C     RANGE GATE SPACING PARAMETERS NEED TO BE CALCULATED FOR EACH
C     BEAM SINCE NUMBER OF GATES CAN CHANGE.
C
      NRG=NRNG
      IF (NRG.GT.MXGDOR) NRG=MXGDOR
      R0=RMIN*0.001
      IF (RNOTUS.NE.0.0) R0=R0+RNOTUS
      RG1=R0

C     Nov. 12, 2002: DRG is in km throughout but GATSPAC and related ID 
C     words are in meters so that, when DRG is set again in other routines, 
C     it is set to either 0.001*GATSPAC or 0.001*ID(33)
C
      DRG=GATSPAC * 0.001
      if(ifrst.eq.1)then
         print *,'DORSWP: changed DRG from m to km for STEPS/SPOL'
         print *,'DORSWP: this should not create problem with airborne'
         print *,'DORSWP: ro,drg,gatspac=',ro,drg,gatspac
         write(7,*)'   DORSWP: nrg,mxgdor,rg1,drg=',nrg,mxgdor,rg1,drg
         write(7,*)'             drgus,rusr2,rmin=',drgus,rusr2,rmin
      endif
      IF (DRGUS.GT.0.0) DRG=DRGUS
      IF (RUSR2.GT.0.0) THEN
         FRAC=AMOD(R0,DRG)
         J=(RUSR1-FRAC)/DRG + 0.999
         IF (J.LT.0) CALL CHKMSG(5,0)
         RG1=J*DRG+FRAC
         NRG=(RUSR2-RG1)/DRG +1.0
         IF(NRG.GT.MXGDOR) NRG=MXGDOR
         RG2=RG1+(NRG-1)*DRG
      ENDIF
      
      RJ1=RMIN
      if(ifrst.eq.1)then
         write(7,*)'   DORSWP: rmin,rnotus,rj1,ibegrg=',
     +        rmin,rnotus,rj1,ibegrg
      endif
      IF (RNOTUS.NE.0.0) RJ1=RJ1+RNOTUS
      NGFH=NRNG
      IF (RUSR2.GT.0.0) THEN
         JL=NINT((RJ1-RG1)/DRG)
         JNG=MIN0(JL+NGFH,NGFH)
         IUNPAD=MAX0(-JL,0)
         IPUTAD=MAX0(JL,0)
         JNG=MIN0(JNG,NRG-IPUTAD)
      ELSE
         IF (RNOTUS.EQ.0.0 .AND. NINT(RJ1).NE.IBEGRG) THEN
            WRITE(*,130)RJ1,IBEGRG
 130        FORMAT(/,5X,'+++CHANGE IN FIRST GATE POS. RJ1=',
     X           F8.2,' IBEGRG=',F8.2,' +++')
            STOP
         END IF
         
         IF (NINT(GATSPAC).NE.ID(33) .AND. DRGUS.EQ.0.0) THEN
C     
C     CHANGE IN RANGE GATE SPACING
C     
            write(*,*)'***gatspac,id(33),drgus=',gatspac,id(33),drgus
            IDRGCHG=IDRGCHG+1
            NRAYS=NRAYS-1
            GDBEAMS=GDBEAMS-1
            BDBEAMS=BDBEAMS+1
            GOTO 100
         END IF
         IUNPAD=0
         IPUTAD=0
         JNG=NRG
      END IF
      J1=1+IPUTAD
      J2=J1+JNG-1
C     
C     LOCATE FIELDS FOR THRESHOLDING
C     
      IF (NTHRSH.GT.0) THEN
c         print *,'NTHRSH=',nthrsh
         DO 132 I=1,NTHRSH
            NUMTH(I)=-1
c            print *,'I,NUMTH=',I,NUMTH(I)
            DO J=1,NFLDDAT
               IEND=INDEX(TFIELD(2,I),' ')
               WRITE(CTEMP,13)FLDNAM(J)
               IF (CTEMP(1:IEND-1).EQ.TFIELD(2,I)(1:IEND-1))THEN
c               IF (TFIELD(2,I)(1:1).EQ.FLDNAM(J)(1:1))THEN
                  NUMTH(I)=J
c                  print *,'Found: J,FLDNAM,TFIELD=',J,' ',FLDNAM(J),' ',
c     +                 TFIELD(2,I),NUMTH(I)
                  GOTO 132
               END IF
c               print *,'       J,FLDNAM,TFIELD=',J,' ',FLDNAM(J),' ',
c     +              TFIELD(2,I),NUMTH(I)
            END DO
            IF (NUMTH(I).EQ.-1) THEN
               WRITE(*,60) TFIELD(2,I)
               print *,'Stopping here'
               STOP
            END IF
 132     CONTINUE
c         DO I=1,NTHRSH
c            print *,'Threshold: I,NUMTH,TFIELD=',I,NUMTH(I),TFIELD(2,I)
c         END DO
      END IF
C     
C     LOCATE REQUESTED FIELDS
C     
c         IDPTR=76
      IDPTR=IDPTR_INT
      DO I=1,NFLDS
         INUM=-1
         DO J=1,NFLDDAT
            IEND=INDEX(REQ_FLDS(I),' ')
            WRITE(CTEMP,13)FLDNAM(J)
 13         FORMAT(A8)
            IF (CTEMP(1:IEND-1).EQ.(IFIELD(I)(1:IEND-1)) .OR.
     X           IFIELD(I)(1:2).EQ.'AZ'   .OR. 
     X           IFIELD(I)(1:2).EQ.'EL'   .OR.
     X           IFIELD(I)(1:4).EQ.'TIME' .OR.
     X           IFIELD(I)(1:6).EQ.'ROTANG')INUM=J
         END DO
c-----------debug (ljm)
         if(ifrst.eq.1)then
            print *,
     +           '   Fld request=',i,' ',req_flds(i),ifield(i),
     +           ' Avail=',inum,' ',fldnam(inum)
         endif
c-----------debug (ljm)
         IF (INUM.EQ.-1) THEN
            WRITE(*,60) REQ_FLDS(I)
 60         FORMAT(/,5X,'+++ FIELD ',A8,'NOT FOUND IN INPUT DATASET',
     X           '+++')
            WRITE(*,70)
 70         FORMAT(5X,'FIELDS PRESENT...')
            DO J=1,NFLDDAT
               WRITE(*,80)FLDNAM(J)
 80            FORMAT(5X,A8)
            END DO
            STOP
         END IF
         IF (IFRST.EQ.1) THEN
            READ(IFIELD(I),400)ID(IDPTR),ID(IDPTR+1)
 400        FORMAT(2A4)
            IFC=IFC+1
            MTFIEL(IFC)=I
            ITYP=ITPFLDC(IFIELD(I))
            
c     All ID values initialized to IBAD in INITAL.f
c
c            ID(IDPTR+2)=0
            IF (ITYP.EQ.1) THEN
               IF (CFAC1.EQ.0.0) THEN
                  ID(IDPTR+2)=RADCON
               ELSE IF (CFAC1.EQ.-32767.) THEN
                  WRITE(*,90)
 90               FORMAT(/,5X,'+++NEED CALIB. INFO FOR DM FIELD+++')
                  STOP
               ELSE
                  ID(IDPTR+2)=CFAC1*100.
               END IF
            ELSE IF (ITYP.EQ.3) THEN
               IF (VNYQ.EQ.0.0) THEN
                  ID(IDPTR+2)=VNYDAT*100.
               ELSE
                  ID(IDPTR+2)=VNYQ*100.
               END IF
            END IF
            print *,'DORSWP: i,fld,ptr,id(0-2)=',i,' ',
     +           ifield(i),idptr,id(idptr),id(idptr+1),id(idptr+2)
         END IF
         K=IUNPAD+1
         IF (FNUM.EQ.0 .OR. (FNUM.GT.0 .AND. RFNAM(1:8).NE.
     X        IFIELD(I))) THEN
            IF (IFIELD(I)(1:2) .EQ. 'AZ') THEN
C     AZIMUTH OF BEAM RELATIVE TO TRUE NORTH (CAN BE USED IN SYNTHESIS)
               IF (IFRST.EQ.1) ID(IDPTR+4)=64
               DO 501 J=1,NRG
                  KOUT(KST+J)=IBAD
                  IF (J.LT.J1 .OR. J.GT.J2) GOTO 501
                  IF (J.GT.NRNG) GOTO 501
                  KOUT(KST+J)=NINT(AZ*ID(IDPTR+4))
                  K=K+1
 501           CONTINUE
            ELSE IF (IFIELD(I)(1:2) .EQ. 'EL') THEN
C     ELEVATION OF BEAM (CAN BE USED IN SYNTHESIS)
               IF (IFRST.EQ.1) ID(IDPTR+4)=100
               DO 502 J=1,NRG
                  KOUT(KST+J)=IBAD
                  IF (J.LT.J1 .OR. J.GT.J2) GOTO 502
                  IF (J.GT.NRNG) GOTO 502
                  KOUT(KST+J)=NINT(EL*ID(IDPTR+4))
                  K=K+1
 502           CONTINUE
            ELSE IF (IFIELD(I)(1:6) .EQ. 'ROTANG') THEN
C     ROTATION ANGLE OF BEAM (CAN BE USED FOR INTERPOLATION CHECK)
               IF (IFRST.EQ.1) ID(IDPTR+4)=64
               DO 503 J=1,NRG
                  KOUT(KST+J)=IBAD
                  IF (J.LT.J1 .OR. J.GT.J2) GOTO 503
                  IF (J.GT.NRNG) GOTO 503
                  KOUT(KST+J)=NINT(ROTANG*ID(IDPTR+4))
                  K=K+1
 503           CONTINUE
            ELSE IF (IFIELD(I)(1:4) .EQ. 'TIME') THEN
C     TIME IN SECONDS FROM BEGINNING OF VOLUME SCAN
               IF (IFRST.EQ.1) THEN
                  BEGIN_SECS=IHR*3600+IMIN*60+ISEC
                  ID(IDPTR+4)=8
c--------------------write(7,*)'Beginning time: ',ihr,imin,isec,begin_secs
               END IF
               DO 504 J=1,NRG
                  KOUT(KST+J)=IBAD
                  IF (J.LT.J1 .OR. J.GT.J2) GOTO 504
                  IF (J.GT.NRNG) GOTO 504
                  SECONDS=BEGIN_SECS - (IHR*3600+IMIN*60+ISEC)
                  KOUT(KST+J)=NINT(SECONDS*ID(IDPTR+4))
                  K=K+1
 504           CONTINUE
            ELSE
C     NORMAL FIELD
               IF (IFRST.EQ.1) THEN
                  ID(IDPTR+4)=100
                  write(7,*)
     +                 '   DORSWP: fld(i),k,kst,nrg,j12=',
     +                 ifield(i),k,kst,nrg,nrng,j1,j2
               END IF
               DO 500 J=1,NRG
CSet the data to the bad flag that Sprint expects.  This is ONLY for
Cinitialization purposes.
                  KOUT(KST+J)=IBAD
                  IF (J.LT.J1 .OR. J.GT.J2) GOTO 500
                  IF (J.GT.NRNG) GOTO 500
                  FDATA = FLDDAT(K,INUM)
CBad is the value returned from the rdbeam routine.  This is the value
Cthat is in the parameter descriptor on the DORADE data set.  
CIBAD is the value that Sprint expects in the interpolation it is -32768.
                  IF(FDATA .EQ. BAD) THEN
                     KOUT(KST+J) = IBAD
                  ELSE
                     KOUT(KST+J) = NINT(FLDDAT(K,INUM)*100.)
                  ENDIF
                  K=K+1
 500           CONTINUE
            END IF
         ELSE
C     
C     ANALYTIC FUNCTION
C
            IF (IFRST.EQ.1)ID(IDPTR+4)=100
            IF(ICOPLANE .EQ. 5) THEN
               CALL ANLACT(FNUM,P1,P2,P3,P4,P10,NRG,KST,J1,
     X              J2,NRNG,ID(IDPTR+4),K,EL,TILT,Y0,RG1,AZ,
     X              DRG,ICOPLANE,ROTANG,XORR,YORR,ZORR)
            ELSE
               CALL ANLREP(FNUM,P1,P2,P3,P4,P10,NRG,KST,J1,
     X              J2,NRNG,ID(IDPTR+4),K,EL,FXANG,
     X              AZ,ICOPLANE,RJ1,DEG,XORR,YORR,
     X              ZORR)
            ENDIF
         END IF
         KST=KST+NRG
         IDPTR=IDPTR+5
      END DO
C     
C     WRITE THE BEAM TO DISK
C
      NLEN=NFLINP*NRG + ID(37)
c      if(mod(nrays,10).eq.0)then
c         write(7,*)'   DORSWP: nrays,nst,ltmp,nlen,ae=',
c     +        nrays,nst,ltmp,nlen,az,el
c      end if
      
c      CALL DMPINTGR(KOUT,NID)
      azim = float(kout(1))/float(jrh6)
      elev = float(kout(2))/float(jrh7)
c      WRITE(7,398) azim,elev, (KOUT(L), L=3,15)
c 398  FORMAT(3X,'A,E=',2f6.1,13I6)
      IFLG=0
      CALL WRRYDK(KPCK,KOUT,NST,LTMP,IFLG,NLEN)
C
C
C     GO READ NEXT BEAM
C
      IFRST=0
      GOTO 100
      
 200  CONTINUE
C     
C     END OF SWEEP - Return mean conditions for the airborne sweep
C
      if(ifd.eq.1)then
         write(7,*)'DORSWP: apparent end-of-sweep, istat=',istat
      end if
      IF(ICOPLANE .EQ. 5) THEN
         ROTEND=ROTKP
         IF (NRAYS.GT.0) THEN
            ALTMEAN  = ALTSUM/NRAYS
            TRCKMEAN = TRCKSM/NRAYS
            DRFTMEAN = DRFTSM/NRAYS
            PTCHMEAN = PTCHSM/NRAYS
            ROLLMEAN = ROLLSM/NRAYS
            SPACMEAN = SPACSM/NRAYS
            TILTMEAN = TILTSM/NRAYS
            ALATMEAN = ALATSM/NRAYS
            ALONMEAN = ALONSM/NRAYS
            
c------debugging statements (ljm)
            if(debug)then
               write(*,1771)nrays,alatmean,alonmean,altmean,
     X              trckmean,drftmean,rollmean,ptchmean,tiltmean
 1771          format(' end sweep: nrays=',i6,' avg ll=',f8.4,
     X              f10.4,' z=',f8.3,'  tdrpt=',f7.2,4f6.2)
            end if
c------debugging statements (ljm)

            PRINT 325,IUN,NSWPS+1,TRCKMN,TRCKMX,TRCKMEAN,ROTBEG,ROTEND,
     X           SPACMN,SPACMX,SPACMEAN,TILTMN,TILTMX,TILTMEAN,NRAYS,
     X           ISTAT
 325        FORMAT(I3,3X,I3,4X,' 1',3X,11F8.2,I6,I6)
            ALAT1 = ALATMEAN
            ALON1 = ALONMEAN
            TILT = TILTMEAN
         ENDIF
      ELSE
         IF (NRAYS.GT.0) THEN
            IF (DIR .GE. 0) THEN
               IDIR = 1
            ELSE
               IDIR = -1
            END IF
            ELMEAN = ELMEAN/NRAYS
C
C     REVERSE DIRECTION OF SCAN FOR RHI SCANS WITHIN 
C     AZIMUTH SECTORS: 000-045, 225-315, and 315-360.
C     
            FXTMP = FXANG + AZCOR
            IF (FXTMP.GT.360.) FXTMP=FXTMP-360.
            IF (ICOPLANE.EQ.4 .AND. 
     X        ((FXTMP.GE.000. .AND. FXTMP.LT.045.) .OR. 
     X         (FXTMP.GE.225. .AND. FXTMP.LT.315.) .OR. 
     X         (FXTMP.GE.315. .AND. FXTMP.LT.360.))) THEN
               IDIR=-IDIR
            END IF

            PRINT 326,IUN,NSWPS+1,IDIR,FXANG,ELMIN,ELMAX,ELMEAN,
     x           BEGAZ,AZ,NRAYS,BDBEAMS,VNYDAT
 326        FORMAT(I3,2X,I3,3X,I2,5X,F8.2,3(2X,F8.2),2(4X,f8.2),
     x           3X,I3,4X,I2,F7.2)  
         END IF
         ALTMEAN =0.0
         TRCKMEAN=0.0
         DRFTMEAN=0.0
         PTCHMEAN=0.0
         TILTMEAN=0.0
         ALATMEAN=0.0
         ALONMEAN=0.0
      END IF

      RETURN
      END
 
