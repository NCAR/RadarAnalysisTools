      SUBROUTINE NEXSWP(IVOL,ISWP,ISTAT,GDBEAMS,IPTR,FLDDAT,IFRST,
     X     NTHRSH,TFIELD,TLIMITS,RFNAM,FNUM,NUFST,NYQUIST,IFD,
     X     IFD_RAYS,VOL_FOUND,JDAY,CNT_ELEV,SUM_ELEV)
C
C     THIS SUBROUTINE READS ONE SWEEP IN NEXRAD LEVEL 2 FORMAT
C     AND OUTPUTS IT TO DISK (WRRYDK) IN A SIMPLIFIED FORMAT.
C-------------------------------------------------------------------
C     NFLDDAT - Number of fields in the current elevation
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
C     GDBEAMS - Beam counter incremented here, GDBEAMS .le. NRAYS.
C     BDBEAMS - Number of beams that have been thrown out.
C-------------------------------------------------------------------
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
c-----PARAMETER (MAXEL=150,NID=129+3*MAXEL)
c-----PARAMETER (NIOB=85000,MAXIN=8500,MAXLEN=MAXIN/4)
c-----PARAMETER (MAXRNG=1024,MAXFLD=16)
c-----DATA LFTIM,JRH6,JRH7,IBAD /0,64,100,-32768/

c      PARAMETER (MAXSKP=27,MXCNT=500)
      COMMON /IO/KPCK(NIOB),KOUT(MAXIN),IBUF(MAXIN),NBUF(MAXLEN,4),
     X     IZ8(17),ILSTREC
      COMMON /IDBLK/ID(NID)
      
      COMMON /CFLD/NFLDS
      COMMON /CFLDC/ IFIELD(MAXFLD), INTINF(MAXFLD,3)
      CHARACTER*8 INTINF,IFIELD

      COMMON /CINP/IUN,ISKP,ORLAT,ORLON
      COMMON /RINP/RADLAT,RADLON,RADALT
      COMMON /CINPC/ NMRAD,ITAP,TAPE_UNIT
      CHARACTER*4 NMRAD
      CHARACTER*8 ITAP
      CHARACTER*8 TAPE_UNIT

      COMMON /UNITS/LUN,LSKP,FEETS
      COMMON /UNITSC/ IPROJ,LTAP,ISCI
      CHARACTER*4 IPROJ
      CHARACTER*8 LTAP,ISCI
      
      COMMON /INITV/ NRNG,RMIN,GATSPAC,IMIN,ISEC
      COMMON /CRNG/ELTUS,MNBEM,RUSR1,RUSR2,RG1,RG2,DRG,NRG,VNYQ,
     X     RNOTUS,DRGUS,CFAC1,CFAC2,CFAC3
      COMMON /CRNGC/ IRCFXL
      CHARACTER*8 IRCFXL
      
      COMMON /CPRO/KDAY,KBTIM,KETIM,IROV,ISWMTH,FXSTOL
      COMMON /CPROC/ IREORD(3)
      CHARACTER*1 IREORD
      
      COMMON /BYTORD/ MBYTE,SWAPPING
      INTEGER SWAPPING
      COMMON /SCRDSK/ LTMP
      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI,ILLE,ILLZ
      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X     ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      
      DATA ATR /0.0174533/

      CHARACTER*8 TFIELD(2,MAXFLD),CTEMP,RFNAM,P10,BLANK

      CHARACTER*8 REQ_FLDS(MXFNEX)
      CHARACTER*8 FLDNAM(MXFNEX)
      DIMENSION FLDDAT(MXGNEX,MXFNEX)
      
      DATA BLANK/'        '/
      
      DIMENSION NUMTH(MAXFLD),MTFIEL(MAXFLD)

      INTEGER NFLDDAT
      INTEGER IYR,IMON,IDAY,IHR,MSEC
      INTEGER GDBEAMS
      INTEGER FNUM
      INTEGER PROCESS
      INTEGER BDBEAMS
      REAL    AZ,EL,ROTANG,FXANG,NYQUIST,UNAMB_RANGE
      REAL    ELMIN,ELMAX,ELMEAN,AZMAX,AZMIN,AZTMP,DELAZ
      REAL    BEGAZ,ENDAZ 
      REAL    SCALE
      LOGICAL VOL_FOUND

c     IBEGREG must be "SAVEd" between calls to NEXSWP
c     or else it becomes a bogus value when compiled 
c     with gfortran.  Older f77 compiler initialized
c     variables to 0 by default.  (LJM - 08/03/2010)
c
      SAVE IBEGRG

      SCALE = 1.0/64.0
c      IDPTR  =76
      IDPTR  =IDPTR_INT
      IFC    =0
      IREWND =0
      ROTKP  =-1000.
      ELMIN  = 1000.
      ELMAX  = 0.0
      ELSUM = 0.0
      NFLINP = NFLDS
      AZMAX = 0.0
      AZMIN = 1000.0
      NRAYS = 0
      GDBEAMS = 0 
      BDBEAMS = 0
      DIR   = 0
      PROCESS = 0
C     
C     CALCULATE RADAR COORDINATES IN ROTATED (IF ROTATED) COORD. SYS.
C     
      IF (ANGXAX.NE.90.0) THEN
         DHETA=(ANGXAX-90.0)*ATR
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
C     
C     LINE 100 IS THE START OF THE LOOP FOR READING A SWEEP'S WORTH OF BEAMS
C
 100  CALL NEXRAD_RDBEAM(NUFST,KETIM,IUN,IREWND,ISTAT,IVOL,  
     X     ISWP,IYR, IMON, IDAY, IHR, IMIN, ISEC, MSEC, JULDAY,
     X     AZ, EL, ROTANG, FXANG, NYQUIST,UNAMB_RANGE,
     X     FLDDAT,FLDNAM,REQ_FLDS,NREQFLDS,NFLDDAT,NRNG,RMIN,GATSPAC,
     X     NRAYS,SWAPPING,TAPE_UNIT,PROCESS)
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
c     Check dimensions of Sprint disk arrays (KPCK, KOUT)
c     See also WRRYDK (KPKBUF, KUNBUF).
c     Need enough to write a beams worth of data to disk.
c
      if(vol_found .or. istat.eq.0)then
         cnt_elev=0.0
         sum_elev=0.0
         write(7,*)'  '
         write(7,*)
         write(7,*)
     +   'Nexswp (bos):  vsw-fxae,maxin=',ivol,iswp+1,fxang,az,el,maxin
         vol_found = .false.
      end if
c-----debug (ljm)

      IF(ISTAT .EQ. 6 .OR. ISTAT .EQ. 7 ) THEN 
         GOTO 200
      ELSE IF(ISTAT .EQ. 5) THEN
         GOTO 100
      ELSE
         GDBEAMS = GDBEAMS + 1
      END IF 
   
C     See DUMP for turning on dumping of every IFD_RAYS housekeeping.
C
      cnt_elev=cnt_elev+1.0
      sum_elev=sum_elev+el
            WRITE(7,771)
     +           IDATE,ITIME,AZ,EL,FXANG,NRNG,R0,NINT(GATSPAC),
     +           NYQUIST,IVOL,ISWP+1,GDBEAMS,NFLDDAT,(FLDNAM(I)(1:2),
     +           I=1,3),ISTAT
      IF(IFD.EQ.1)THEN
         IF(ISTAT .EQ. 0 .OR.
     +      ISTAT .EQ. 2 .OR.
     +      ISTAT .EQ. 3 .OR.
     +      ISTAT .EQ. 4 .OR. 
     +      GDBEAMS .EQ. 1 .OR.
     +      MOD(GDBEAMS,IFD_RAYS).EQ.0)THEN
            IDATE = 10000*IYR + 100*IMON + IDAY
            ITIME = 10000*IHR + 100*IMIN + ISEC
            R0=0.001*RMIN
            WRITE(7,771)
     +           IDATE,ITIME,AZ,EL,FXANG,NRNG,R0,NINT(GATSPAC),
     +           NYQUIST,IVOL,ISWP+1,GDBEAMS,NFLDDAT,(FLDNAM(I)(1:2),
     +           I=1,3),ISTAT
 771        FORMAT(1X,'D=',I6,' T=',I6.6,' A=',F5.1,' E=',F5.2,
     +           ' Fx=',F5.2,' Ng=',I4,' Rmn=',F7.3,' Gs=',I4,
     +           ' Ny=',F5.2,' Vl=',I2,' Sw=',I2,' Nb=',I3,
     +           ' Nf=',I1,' Names=',3(A2,1X),' Istat=',I1)
         END IF
      END IF

C     Discard beams that exceed elevation angle tolerance.
C
      IF(ISWMTH.EQ.1)THEN
         IF(ABS(EL-FXANG).GT.FXSTOL)THEN
            GDBEAMS=GDBEAMS-1
            BDBEAMS=BDBEAMS+1
            GO TO 100
         END IF
      END IF

      KST = ID(37)
      
C ANGLES AZIMUTH AND ELEVATION
      KOUT(1) = NINT(AZ * JRH6) 
      KOUT(2) = NINT(FXANG * JRH7)
      IF(EL .LT. ELMIN) ELMIN = EL
      IF(EL .GT. ELMAX) ELMAX = EL
      ELSUM = EL + ELSUM
      IF (ISWP.GE.0 .AND. GDBEAMS.EQ.1) THEN
         ID(IPTR) = NINT(FXANG * JRH7)
         BEGAZ = AZ
         PAZ   = AZ
         GDBEAMS = 1
      END IF
      AZTMP = AZ
      IF (AZTMP.GT.360.0) AZTMP=AZTMP-360.0
      IF (AZTMP.LT.0.0)   AZTMP=AZTMP+360.0
      DIF=AZTMP - PAZ   
      PAZ = AZTMP
      IF (ABS(DIF).GT.180) DIF=DIF-SIGN(360.0,DIF)
      ABDIF=ABS(DIF)
      IF (ABDIF.GT.AZMAX) AZMAX=ABDIF
      IF (ABDIF.LT.AZMIN .AND. ABDIF.GT.0.0) AZMIN=ABDIF
      DIR=DIR+DIF 
      
C THE RAY TIME.
C     Increase the hour by 24 to allow for crossing 00 hour within a volume scan;
C     otherwise, the TIME field will be incorrect.  Only one day-change allowed.
C           JDAY - day of the first ray within the current volume scan
C           IDAY - day of current ray within the current volume scan
C
      KOUT(3)= IHR
      IF (IDAY.NE.JDAY)THEN
         IHR = IHR + 24
         KOUT(3)=KOUT(3)+24
      END IF
c      print *,'NEXSWP: jday,imon,iday,hms=',
c     x     jday,imon,iday,ihr,imin,isec
      KOUT(4)= IMIN
      KOUT(5)= ISEC
      ID(7)  = KOUT(3)
      ID(8)  = KOUT(4)
      ID(9)  = KOUT(5)

C SCALING FACTORS
      KOUT(6)=JRH6
      KOUT(7)=JRH7
      
C     Since the number of gates decreases as the elevation angle increases,
C     set this information for values from the fist sweep in the volume.
C     The rest of Sprint requires equal numbers of range gates for all beams.
C
c      print *,'NEXSWP: ifrst,ibegrg,rmin,gatspac,nrng=',
c     +     ifrst,ibegrg,rmin,gatspac,nrng
      IF(IFRST .EQ. 1) THEN
         IBEGRG  = RMIN * 0.001
         ID(33)  = NINT(GATSPAC)
         NRG=NRNG
         IF (NRG.GT.MXGNEX) NRG=MXGNEX
      ENDIF
      KOUT(8) = NRG
c      print *,'NEXSWP: ifrst,ibegrg,rmin,gatspac,nrng=',
c     +     ifrst,ibegrg,rmin,gatspac,nrng

C LATITUDE AND LONGITUDE OF THE RADAR 
      KOUT(9) =NINT(RADLAT*1000.)
      KOUT(10)=NINT(RADLON*1000.)
      KK=10
C         
C     RANGE GATE SPACING PARAMETERS NEED TO BE CALCULATED FOR EACH
C     BEAM SINCE NUMBER OF GATES CAN CHANGE.
C
c-----NRG=NRNG
c-----IF (NRG.GT.MXGNEX) NRG=MXGNEX
      R0=RMIN*0.001
      IF (RNOTUS.NE.0.0) R0=R0+RNOTUS
      RG1=R0
      DRG=GATSPAC * 0.001
      IF (DRGUS.GT.0.0) DRG=DRGUS
      IF (RUSR2.GT.0.0) THEN
         FRAC=AMOD(R0,DRG)
         J=(RUSR1-FRAC)/DRG + 0.999
         IF (J.LT.0) CALL CHKMSG(5,0)
         RG1=J*DRG+FRAC
         NRG=(RUSR2-RG1)/DRG + 1.0
         IF(NRG.GT.MXGNEX) NRG=MXGNEX
         RG2=RG1+(NRG-1)*DRG
      ENDIF
      
      RJ1=RMIN*0.001
      IF (RNOTUS.NE.0.0) RJ1=RJ1+RNOTUS
c      print *,'NEXSWP: rmin,rj1,rnotus,ibegrg=',rmin,rj1,rnotus,ibegrg
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
      
C     CHECK FOR A CHANGE IN RANGE GATE SPACING
C
         IF (NINT(GATSPAC).NE.ID(33) .AND. DRGUS.EQ.0.0) THEN
            PRINT *,'***GATSPAC,ID(33),DRGUS=',GATSPAC,ID(33),DRGUS
            IDRGCHG=IDRGCHG+1
            GDBEAMS=GDBEAMS-1
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
         DO 132 I=1,NTHRSH
            NUMTH(I)=-1
            DO J=1,NFLDDAT
               IF (TFIELD(2,I).EQ.FLDNAM(J)) NUMTH(I)=J
               GOTO 132
            END DO
            IF (NUMTH(I).EQ.-1) THEN
               WRITE(*,60) TFIELD(2,I)
               STOP
            END IF
 132     CONTINUE
      END IF
C
C     LOCATE REQUESTED FIELDS
C
c      IDPTR=76
      IDPTR=IDPTR_INT
      DO I=1,NFLDS
         INUM=-1
         DO J=1,NFLDDAT
            IEND=INDEX(IFIELD(I),' ')
            WRITE(CTEMP,13)FLDNAM(J)
 13         FORMAT(A7)
            IF (FLDNAM(J)(1:2) .EQ. IFIELD(I)(1:2) .OR.
     X          IFIELD(I)(1:2) .EQ. 'AZ' .OR. 
     X          IFIELD(I)(1:2) .EQ. 'EL' .OR.
     X          IFIELD(I)(1:4) .EQ. 'TIME')INUM=J
         END DO
         IF (INUM.EQ.-1) THEN
            WRITE(*,60) IFIELD(I), EL
 60         FORMAT(/,5X,'+++ FIELD ',A8,'NOT FOUND IN NEXRAD INPUT',
     X           ' DATASET AT ELEVATION ANGLE = ',F6.2,' +++')
            WRITE(*,70)
 70         FORMAT(/,5X,'FIELDS PRESENT...')
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
            ID(IDPTR+2)=0
            IF (ITYP.EQ.1) THEN
               IF (CFAC1.EQ.0.0) THEN
                  ID(IDPTR+2)=-1
               ELSE IF (CFAC1.EQ.-32767.) THEN
                  WRITE(*,90)
 90               FORMAT(/,5X,'+++NEED CALIB. INFO FOR DM FIELD+++')
                  STOP
               ELSE
                  ID(IDPTR+2)=CFAC1*100.
               END IF
            ELSE IF (ITYP.EQ.3) THEN
               IF (VNYQ.EQ.0.0) THEN
                  ID(IDPTR+2)= NYQUIST*100.
                  VNYQ=NYQUIST
               ELSE
                  ID(IDPTR+2)=VNYQ*100.
               END IF
            END IF
         END IF
         K=IUNPAD+1
c-----debug (ljm) 
c         write(7,1770)gdbeams,az,nrg,nrng,kst
c 1770    format(1x,'Nexswp: gdbeams,az,nrg,nrng,kst',i8,f8.3,3i8)
c-----debug (ljm) 

         IF (FNUM.EQ.0 .OR. (FNUM.GT.0 .AND. RFNAM(1:8).NE.IFIELD(I)))
     X        THEN
            IF (IFIELD(I)(1:2) .EQ. 'AZ') THEN
C     AZIMUTH OF BEAM RELATIVE TO TRUE NORTH (CAN BE USED IN SYNTHESIS)
               IF (IFRST.EQ.1) ID(IDPTR+4)=64
               DO 501 J=1,NRG
                  KK=KK+1
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
                  KK=KK+1
                  KOUT(KST+J)=IBAD
                  IF (J.LT.J1 .OR. J.GT.J2) GOTO 502
                  IF (J.GT.NRNG) GOTO 502
                  KOUT(KST+J)=NINT(EL*ID(IDPTR+4))
                  K=K+1
 502           CONTINUE
            ELSE IF (IFIELD(I)(1:4) .EQ. 'TIME') THEN
C     TIME IN SECONDS FROM BEGINNING OF VOLUME SCAN
               IF (IFRST.EQ.1) THEN
                  BEGIN_SECS=IHR*3600+IMIN*60+ISEC
                  ID(IDPTR+4)=8
               END IF
               DO 503 J=1,NRG
                  KK=KK+1
                  KOUT(KST+J)=IBAD
                  IF (J.LT.J1 .OR. J.GT.J2) GOTO 503
                  IF (J.GT.NRNG) GOTO 503
                  SECONDS=BEGIN_SECS - (IHR*3600+IMIN*60+ISEC)
                  KOUT(KST+J)=NINT(SECONDS*ID(IDPTR+4))
                  K=K+1
 503           CONTINUE
            ELSE
C     NORMAL FIELD
               IF (IFRST.EQ.1) ID(IDPTR+4)=100
               DO 500 J=1,NRG
                  KK=KK+1
                  KOUT(KST+J)=IBAD
                  IF (J.LT.J1 .OR. J.GT.J2) GOTO 500 
                  IF (J.GT.NRNG) GOTO 500 
                  X = NINT(FLDDAT(K,INUM)*100.)
c--temporarily set missing DZ to -15.0 dBZ (ljm 2/9/2000)
c                  IF (IFIELD(INUM)(1:2) .EQ. 'DZ' .AND. 
c     X                X .EQ. IBAD) THEN
c                     X = -1500.0
c                  END IF
c--are 0 and 1 really bad values (ljm 2/4/2000) - don't seem to be
c                  IF (X .EQ. 0) X = IBAD
c                  IF (X .EQ. 1) X = IBAD
c-----------------if(j .lt. 10) print *,x
                  KOUT(KST+J) = X
                  K=K+1
 500           CONTINUE
            END IF
         ELSE
C
C     REPLACE FIELD RFNAM WITH AN ANALYTIC FUNCTION
C
            print *,'NEXSWP replace rfnam with # =',rfnam,fnum
            IF (IFRST.EQ.1) ID(IDPTR+4)=100
            CALL ANLREP(FNUM,P1,P2,P3,P4,P10,NRG,KST,J1,
     X           J2,NRG,ID(IDPTR+4),K,EL,FXANG,AZ,ICOPLANE,
     X           RJ1,DRG,XORR,YORR,ZORR)
         END IF
         KST=KST+NRG
         IDPTR=IDPTR+5
         
      END DO 
C
C     CHECK IF THIS IS THE FIRST SWEEP TO BE PROCESSED; IF SO GET RANGE INFO 
C     FROM IT
C
      IF (IFRST.EQ.1) THEN
         IFRST = 0
         CALL INITVOL(IPROJ)
      END IF
C
C     WRITE THE BEAM TO DISK
C
      NLEN=NFLINP*NRG + ID(37)
      CALL WRRYDK(KPCK,KOUT,NST,LTMP,0,NLEN)
C     Subroutine WRRYDK(KPKBUF,KUNBUF,NST,IUN,IFLG,NLEN)
C
C     IF END OF SWEEP OR END OF VOLUME THEN PROCESS
C
      IF(ISTAT .EQ. 2 .OR. ISTAT .EQ. 4 ) GOTO 200

C WE HAVE NOT READ AN END OF VOLUME OR SWEEP SO READ ANOTHER BEAM
      GOTO 100
      
 200  CONTINUE
      IF((ISTAT .EQ. 7) .AND. (GDBEAMS .LT. 2)) THEN
         STOP
      END IF
      
      IF(GDBEAMS .GT. 0) THEN
         ELMEAN = ELSUM/GDBEAMS
         DELAZ  = ABS(DIR)/GDBEAMS
         ID(IPTR+1)=DIR/ABS(DIR)
      ELSE
         ELMEAN = 0.0
         DELAZ  = 0.0
      END IF         

      IF(ISTAT .NE. 6) THEN 
         ENDAZ  = AZ
         ISWP   = ISWP + 1
         PRINT 425,ISWP,ID(IPTR+1),NYQUIST,FXANG,ELMIN,ELMAX,
     X        ELMEAN,BEGAZ,ENDAZ,AZMIN,AZMAX,DELAZ,GDBEAMS,
     X        BDBEAMS,NRAYS
 425     FORMAT(2(4X,I2),F8.2,2X,4(F6.2,3X),4X,2(F6.1,4X),
     X        3(F6.2,3X),I7,I6,I6)
      END IF
      
C     Before leaving NEXRAD, set NRNG to value (NRG)
C     from the first sweep in the current volume.
C
      NRNG=NRG
      RETURN
      END
