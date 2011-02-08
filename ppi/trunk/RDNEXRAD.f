      SUBROUTINE RDNEXRAD(IUN,IREWIND,DEC,DECWR,WORDSZ,IFD,NRST,
     X     NDUMP,IBSWEP,IESWEP,ZSTR,PLTSW,COLRFIL,VECTS,NFRAME,
     X     LABLS,IGRPLT,BGFLAG,IVOL,IVOLOLD,ITIMBOV,ITIMEOV,AZVOL,
     X     ELVOL,NBMAX,NBVOL,NFXVOL,IEOS,IEOV,IEOF,NAMNEX,NFL_NEX,
     X     NEWDAY)
C
C     ROUTINE TO READ A SCAN OF DATA IN THE NCDC NEXRAD LEVEL II
C     ARCHIVE FORMAT.        
C------------------------------------------------------------------------*
C     NEXRAD - LEVEL II Tape Documentation
C     See http://www.ncdc.noaa.gov/ol/radar/leveliidoc.html#RECORDS
C
C     Within the data file, base data and control/response messages are 
C     stored using a variable record-length structure.  The convention
C     is to begin with byte 0 as the first byte.  Included as the first
C     record of each data file is a volume scan title containing the 
C     following information.
C
C     Bytes  Format   Description
C     0-8    Char*9   Filename (root) = "ARCHIVE2."
C     9-11   Char*3   Filename (extension) = "1", "2", etc.
C     12-15  Int*4    Modified Julian Date referenced from 1/1/70
C     16-19  Int*4    Time - Milliseconds from midnight (UTC) of the day
C                     when the file was created.
C     20-23           Unused
C
C     All remaining records in the data file are composed of data and 
C     command/response messages which are initially stored in separate
C     2432 byte packets within an RDA memory buffer.  During the archive
C     process the packets are copied from memory and grouped together to
C     form a record.  Record lengths are variable and are always sized in
C     multiples of the 2432 byte packets.  During the reblocking process,
C     physical records are set to 31616 bytes (2432 x 13).
C------------------------------------------------------------------------*
C     Note: (MXG_NEX,MXF_NEX) in dim.inc and 
C           (MXGAT,MXFLD) in nexrad.h must be the same
C
C     NREC    - Record counter within a sweep (NREC .ge. NAZ)
C     IPREC   - Record counter from beginning of data on current unit
C     NTANG   - Total number of beams in a sweep, including transition
C     NAZ     - Total number of good beams in a sweep
C     NBAD    - Number of discarded beams
C     NANG(1) - Number of good beams
C     IBTIME  - Requested beginning time for processing
C     IETIME  -     "     ending      "   "       "
C     IFTIME  - Beginning time of sweep
C     ITIME   - Current     "   "   "   (also ending time)
C     NFLDS   - Number of requested fields
C     NAMFLD  - Names   "     "        "
C     IFLD    -         "     "        "
C     NDUMP   - Number of beams per sweep to do full dumps
C
C     PRINTI   - Flag used for printing information in the C routines.  
C                (0) Don't , (1) Do print.
C     IFD      - Flag used for printing beam information here.
C                (0) Don't , (1) Do print every NRSTh beam.
C     IEOS     - Flag for end-of-sweep:  (1) End of sweep
C     IEOV     - Flag for end-of-volume: (1) End of volume
C     IEOF     - Flag for end-of-file:   (1) End of file
C     IEOT     - Flag for end-of-data:   (1) End of data - see data.inc
C
C     Note: Values in arrays are ordered DZ, VE, and SW.
C
C     NFL_NEX  - Number of NEXRAD fields in the beam
C     FLDDAT   - Two dimensional array containing a beam of DZ, VE, and SW
C                field values.  These values have been scaled and offset in 
C                the nexrad.c routine, with missing data set to -32768.
C     NRNG     - Number of range gates for DZ, VE, AND SW.
C     RMIN     - Ranges (m) to the first gate for DZ,VE AND SW.  Set to 
C                -32768 when the field is not present in the current sweep.
C     GATSPAC  - Gate spacings (m) for DZ, VE, and SW.  Set to -32768 when
C                the field is not present in the current sweep.
C     TAPE_UNIT- Place holder for now and could contain the name of a tape 
C                device if reading data from physical tape rather than disk.
C
      INCLUDE 'dim.inc'
      INCLUDE 'swth.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'

      PARAMETER (AINMN=350.0/MXA)

      COMMON/INPUTCH/NAMFLD(MXF),IRATYP,ICORD
      COMMON/ANGLS/ASCAN(MXA),ANGINC(MXA),FXELC(MXA),FXERR(MXA),AVGI

      DIMENSION FLDDAT(MXG_NEX,MXF_NEX)
      DIMENSION DZ(MXG_NEX),VE(MXG_NEX),SW(MXG_NEX)
      DIMENSION RDZ(MXR),RVE(MXR),RSW(MXR),RRNG(MXR)
      DIMENSION AZVOL(NBMAX),ELVOL(NBMAX)

      CHARACTER*8 NAMFLD,IRATYP,ICORD,TAPE_UNIT
      CHARACTER*8 NAMNEX(MXF_NEX)
      CHARACTER*3 ISCTP(8)

      CHARACTER*3 LABLS
      CHARACTER*3 WHY
      CHARACTER*1 BGFLAG
      LOGICAL COLRFIL,FRSTREC,PLTSW,VECTS

      INTEGER ISTAT,JSTAT,SWAPPING
      REAL GATSPAC(MXF_NEX),RMIN(MXF_NEX)
      REAL AZ,EL,VNYQ,FXANG
      INTEGER NRNG(MXF_NEX)
      INTEGER IVOL,ISWP,IYR,MON,DAY,HR,MIN,SEC,MSEC
      INTEGER STARTTIME(4)
      INTEGER JULDAY,NFLDS,NUM_RAYS,PRINTI
      DATA ISCTP/'PPI','COP','RHI','VER','TAR','MAN','IDL','SUR'/
      DATA BADNEX/-32768./
      DATA IOLDDAT/999999/

      TAPE_UNIT = '       '
      IERROR=0
      ITP =8
      MDUMP=NDUMP

C     Set SWAPPING flag if byte-swapping is needed 
C        DEC   - (1) Reading input on DEC, (0) Reading input on non-DEC
C        DECWR - (1) Input written on DEC, (0) Input written on non-DEC
C     Byte-swapping required if (DEC=1 and DECWR=0) or (DEC=0 and DECWR=1).
C
      IF((DEC.EQ.1.0 .AND. DECWR.EQ.0.0).OR.
     X   (DEC.EQ.0.0 .AND. DECWR.EQ.1.0))THEN
         SWAPPING = 1
      ELSE
         SWAPPING = 0  
      END IF

c-----debug (ljm)
c      print *,'RDNEXRAD: ibtime,ietime,iftime,itime=',
c     X     ibtime,ietime,iftime,itime
c      print *,'itp=',itp,itpflg(itp),fxmn(itp),fxmx(itp),naz
c      print *,'ibswep,ieswep,=',ibswep,ieswep
c      print *,'Namflds=',(namfld(n),n=1,nflds)
c      print *,'   Ifld=',(ifld(n),n=1,nflds)
c      print *,'Ifd,ndp=',ifd,nrst,ndump
c-----debug (ljm)

 5    FXOLD  = -99.
      ITPOLD = -99
      DROLD  = -99.
      ISWPOLD= -99
      AZSUM  = 0.0
      AVGI   = 0.0
      NREC   = 0
      NTANG  = 0
      NAZ    = 0
      NBAD   = 0
      ISTAT  = 0
      JSTAT  = 0

c     Changed PRINTI from 0 --> 1 to create printout in C routine
c

      PRINTI = 0
      IEOS   = 0
      IEOV   = 0
      IEOF   = 0
      ISW    = 1

 10   CALL NEXRAD_RDBEAM(IUN,IREWIND,ISTAT,SWAPPING,TAPE_UNIT,
     X                   NAMNEX,FLDDAT,GATSPAC,RMIN,NRNG,
     X                   IVOL,ISWP,IYR,MON,DAY,HR,MIN,SEC,
     X                   MSEC,JULDAY,AZ,EL,FXANG,VNYQ,
     X                   NFL_NEX,NUM_RAYS,PRINTI)

C     Increment counters for records within a sweep and from beginning on current unit
C
      NREC=NREC+1
      IPREC=IPREC+1

C-----------------------------------------------------------
C     Output of istat variable:
C        ISTAT=0  --> START OF NEW SWEEP    - Extract data and go back to 10
C        ISTAT=1  --> INTERMEDIATE BEAM     -    "      "   "  go back to 10
C        ISTAT=2  --> END OF A SWEEP        -    "      "   "  return
C        ISTAT=3  --> START OF A NEW VOLUME -    "      "   "  go back to 10
C        ISTAT=4  --> END OF A VOLUME SCAN  -    "      "   "  return
C
C        ISTAT=5  --> A STATUS DESCRIPTOR WAS FOUND;
C                     THESE CONTAIN NO BEAM DATA - Do not process; go back to 10
C        ISTAT=6  --> END OF DATA                - Wrap up and return
C        ISTAT=7  --> READ ERROR                 - Do not process; go back to 10

      IF(ISTAT.EQ.3 .AND. JSTAT.EQ.3)ISTAT = 6
      IF(ISTAT.EQ.4 .AND. JSTAT.EQ.4)ISTAT = 6

C     Set type-of-beam flag processing according to beam read status
C
      IF(ISTAT.EQ.0)THEN
         WHY='bsw'
      ELSE IF(ISTAT.EQ.1)THEN
         WHY='   '
      ELSE IF(ISTAT.EQ.2)THEN
         WHY='esw'
      ELSE IF(ISTAT.EQ.3)THEN
         WHY='bvl'
      ELSE IF(ISTAT.EQ.4)THEN
         WHY='evl'
      ELSE IF(ISTAT.EQ.5)THEN
         WHY='des'
      ELSE IF(ISTAT.EQ.6)THEN
         WHY='eod'
      ELSE IF(ISTAT.EQ.7)THEN
         WHY='err'
      END IF
c      print *,'RDNEXRAD: iprec,istat,jstat,why=',iprec,istat,jstat,why
c      print *,'RDNEXRAD: nfl_nex,nflds,namnex=',nfl_nex,nflds,namnex
c      print *,'RDNEXRAD:           nrng=',nrng

      IF(ISTAT.EQ.5)THEN

C        Toss out status descriptor record and read another record

         IDATE=BDVAL
         ITIME=BDVAL
         AZ   =BDVAL
         EL   =BDVAL
         FXANG=BDVAL
c--------debug (ljm)
c         IF(IFD.EQ.1)THEN
c            WRITE(6,11)IUN,IPREC
c 11         FORMAT(8X,' RDNX: STATUS DESCRIPTOR ON UNIT= ',I3,
c     X           ' RECORD=',I8)
c         END IF
c--------debug (ljm)
         GO TO 10

      ELSE IF(ISTAT .EQ. 6) THEN

C        End-of-data: Wrap up and return to calling routine

         WRITE(6,15)IUN,IPREC
 15      FORMAT(8X,' RDNX: END OF DATA ON UNIT= ',I3,' RECORD=',I8)
         IEOF=0
         IEOT=1
         RETURN

      ELSE IF(ISTAT .EQ. 7) THEN 

C        Error: toss out record unless 10 in a row ==> End-of-data
C               then wrap up and return to calling routine

         IERROR=IERROR+1
         WRITE(6,17)IUN,IPREC
 17      FORMAT(8X,' RDNX: ERROR READING DATA ON UNIT= ',I3,
     X        ' RECORD=',I8)
         IF(IERROR.GE.10)THEN
            IEOF=0
            IEOT=1
            RETURN
         ELSE
            GO TO 10
         END IF
      END IF
      
      IF(ISTAT.LE.4)THEN

C        Potentially good record: Extract housekeeping information

         JSTAT = ISTAT
         IF(NFL_NEX.GT.MXF_NEX)THEN
            PRINT *,'*** ERROR - NO. NEXRAD FIELDS EXCEEDS *** ',MXF_NEX
            STOP
         END IF

C        Store current beam azimuth and elevation angle information

         NTANG = NTANG+1
         NAZ   = NAZ+1
         AZA(NAZ,1)=AZ
         ELA(NAZ,1)=EL
         IF(NAZ.GT.1)THEN
            AZDIF=ABS(AZ-AZA(NAZ-1,1))
            IF(AZDIF.GT.180.0)AZDIF=360.0-AZDIF
            AZSUM=AZSUM+AZDIF
         END IF

C        Store current beam date and time information
C        If midnight is crossed, set NEWDAY=1 so 
C        that 24 hrs will be added to input times.

         IMON  = MON
         IDAY  = DAY
         IDATE = DAY + 100*MON + 10000*IYR
         IF(IOLDDAT.EQ.999999)IOLDDAT=IDATE
         IF((IDATE-IOLDDAT).EQ.1)THEN
            NEWDAY=1
            HR=HR+24
            WRITE(6,1770)IOLDDAT,IDATE,NEWDAY,HR,MIN,SEC
 1770       FORMAT(1X,'RDNX: IOLDDAT,IDATE,NEWDAY=',3I8,
     X           ' TIME=',I2.2,':',I2.2,':',I2.2)
         END IF
         IOLDDAT=IDATE
         ITIME = SEC + 100*MIN + 10000*HR
         ITM(NAZ,1)=ITIME

C        Reset volume begin time if NFXVOL=0 or store current time.

         IF(NFXVOL.EQ.0)THEN
            ITIMBOV=ITIME
         ELSE
            ITIMEOV=ITIME
         END IF

C        Store time, azimuth, and elevation for volume scan
C        (See PLTAE or PLTHZ)
C
         NBVOL=NBVOL+1
         IF(NBVOL.LE.NBMAX)THEN
            IF(NBVOL.EQ.1)THEN
               ITIMBOV=ITIME
               IVOLOLD=IVOL
            ELSE
               ITIMEOV=ITIME
            END IF
            AZVOL(NBVOL)=AZ
            ELVOL(NBVOL)=EL
         ELSE
            NBVOL=NBMAX
         END IF

C        Store current range and gate information

         NGTSDZ = NRNG(1)
         IDRDZ  = GATSPAC(1)
         DRDZ   = GATSPAC(1)/1000.0
         NGTSVE = NRNG(2)
         IDRVE  = GATSPAC(2)
         DRVE   = GATSPAC(2)/1000.0
         IF(NGTSVE .EQ. 0)THEN
            NGDZ = 4*(NGTSDZ-1)+1
            NGTS = MIN0(NGDZ,MXR)
            DR   = 0.25*DRDZ
            RMN  = RMIN(1)
            R0   = RMIN(1)/1000.0
         ELSE
            NGTS = MIN0(NGTSVE,MXR)
            DR   = DRVE
            RMN  = RMIN(2)
            R0   = RMIN(2)/1000.0
         END IF
         IF(NGTS .GT. MXG_NEX)THEN
            PRINT *,'*** WARNING - NO. NEXRAD GATES EXCEEDS *** ',
     +           MXG_NEX
            NGTS=MXG_NEX
         END IF

         IDR   = NINT(1000.0*DR)

      END IF

      IF(ISTAT.EQ.0 .OR. ISTAT.EQ.3)THEN

C        Beginning of a new sweep or volume
C
         STARTTIME(1) = HR
         STARTTIME(2) = MIN
         STARTTIME(3) = SEC
         STARTTIME(4) = MSEC
         IEOS   = 0
         IEOV   = 0
         IEOF   = 0
         NREC   = 1
         NTANG  = 0
         NAZ    = 0
         NBAD   = 0
         IFTIME = ITIME
         FXOLD  = FXANG
         ITPOLD = ITP
         ISWPOLD= ISWP
         NGTSOLD= NGTS
         DROLD  = DR
         IF(ISTAT.EQ.3)IVOLOLD=IVOL

c--------debug (ljm)
c         print *,'Rng: ngtsold,r0,drold=',ngtsold,r0,drold
c         do i=1,ngtsold
c            rrng(i)=rng(i,1)
c         end do
c         write(6,33)0,'Rg',ngtsold,ngts,rmn,r0
c         call dmpfloat(rrng,ngts)
c--------debug (ljm)

         IF(DROLD.NE.0.0)THEN
            CALL RNGST
            CALL MNMX(DROLD)
         END IF

         IF(IFD.EQ.1)THEN

C           Dump first beam of sweep
C
            WRITE(6,19)
 19         FORMAT(/,1X,'Begin Sweep')

            WRITE(6,21)IDATE,ITIME,AZ,EL,FXANG,ISCTP(ITP)(1:1),
     X           NGTSDZ,NGTSVE,NGTS,IDRDZ,IDRVE,IDR,VNYQ,NREC,
     X           ISTAT,WHY
 21         FORMAT(1X,' D=',I6.6,' T=',I6.6,' A=',F6.2,' E=',F5.2,
     X           ' Fx=',F5.2,' M=',A1,' Ng(Z,V)=',2I4,I5,
     X           ' Dr(Z,V)=',3I5,' Nyq=',F5.2,
     X           ' Rec=',I3,' Ist=',I1,A3)
         END IF

      ELSE IF(ISTAT.EQ.2 .OR. ISTAT.EQ.4)THEN
      
C        End of a sweep or volume
C
         IEOS=1
         IF(ISTAT.EQ.4)IEOV=1

         IF(IFD.EQ.1)THEN

C           Dump last beam of sweep
C
            WRITE(6,21)IDATE,ITIME,AZ,EL,FXANG,ISCTP(ITP),NGTSDZ,
     X           NGTSVE,NGTS,IDRDZ,IDRVE,IDR,VNYQ,NREC,ISTAT,
     X           WHY
            WRITE(6,27)
 27         FORMAT(1X,'End Sweep',/)
         END IF
         GO TO 30

      END IF

      IF(ISTAT.LE.3)THEN
         
C        Check if this record (beams worth of data) should be processed.
C        Process only if: (1) after the requested time, (2) the desired 
C        scan type, (3) within the range of sweep numbers, (4) the right
C        fixed angle, or (5) within angle tolerances.  Failure of any one
C        of these is enough to prevent further processsing.  Read another
C        record (Go back to 10).
C

C     ***Check if before requested time***
         IF((ITIME+NEWDAY*240000).LT.IBTIME)WHY='-bt'
      
C     ***Check scan type***
         IF(ITPFLG(ITP).EQ.0)WHY='-st'

C     ***Check sweep number***
         IF(ISWP.LT.IBSWEP.OR.ISWP.GT.IESWEP)WHY='-sn'

C     ***Check fixed angles***
         IF(FXANG.LT.FXMN(ITP).OR.FXANG.GT.FXMX(ITP))WHY='-fx'

C     ***Check angle tolerance***
         IF(ABS(FXANG-EL).GT.ANGTOL(ITP))WHY='-at'

         IF(WHY(1:1).EQ.'-')THEN

C     ***Failed one of the tests: don't process the beam
C
            NBAD=NBAD+1
            NAZ =NAZ-1
            IF(NAZ.LT.0)NAZ=0
            IF( IFD.EQ.1 .AND. MOD(NREC,NRST).EQ.0)THEN
               
C              Dump every NRST-th intermediate beam that won't be processed
C
               WRITE(6,21)IDATE,ITIME,AZ,EL,FXANG,ISCTP(ITP),NGTSDZ,
     X              NGTSVE,NGTS,IDRDZ,IDRVE,IDR,VNYQ,NREC,ISTAT,
     X              WHY
            END IF
            GO TO 10
         END IF

      END IF

      IF( IFD.EQ.1 .AND. NRST.NE.0 .AND. NREC.NE.1 
     X        .AND. MOD(NREC,NRST).EQ.0)THEN

C        Dump every NRST-th intermediate beam that will be processed
C
         WRITE(6,21)IDATE,ITIME,AZ,EL,FXANG,ISCTP(ITP),NGTSDZ,
     X        NGTSVE,NGTS,IDRDZ,IDRVE,IDR,VNYQ,NREC,ISTAT,
     X        WHY
      END IF

 30   CONTINUE

C     Continue the processing of beams
C
      
c-----print *,'ITIME,NEWDAY,IETIME=',itime,newday,ietime
      IF((ITIME+NEWDAY*240000).GT.IETIME)THEN

C        Past requested processing time: return to calling routine

         WRITE(6,31)IETIME,IUN,IPREC
 31      FORMAT(8X,' RDNX: Past requested processing time ',I6.6,
     X        ' on Unit= ',I3,' Record=',I8,/)
         IEOF=1
         IEOV=1
         IEOT=1
         GO TO 110
      END IF

C-----------------------------------------------------------
C     LOAD THE DZ, VE, and SW ARRAYS FOR THIS BEAM
C
      IF(NGTSDZ .GT. 0) THEN
         DO I=1,NGTSDZ
            DZ(I) = FLDDAT(I,1)
         END DO
      END IF
      IF(NGTSVE .GT. 0) THEN
         DO I = 1,NGTSVE
            VE(I) = FLDDAT(I,2)
            SW(I) = FLDDAT(I,3)
         END DO
      END IF

C     Dump MDUMP beams worth of data before filling DZ to VE resolution
C
      IF(IFD.EQ.1 .AND. MDUMP.GT.0)THEN
         DO N=1,NFL_NEX
            WRITE(6,33)N,NAMNEX(N),NRNG(N),NGTS,RMIN(N),R0
 33         FORMAT(1X,' N,NAMNEX=',I6,2X,A8,2X,'NGTS=',2I8,3X,
     X           'RMIN=',2F8.3)
            IF(N.EQ.1 .AND. NRNG(N).GT.0)CALL DMPFLOAT(DZ,NRNG(N))
            IF(N.EQ.2 .AND. NRNG(N).GT.0)CALL DMPFLOAT(VE,NRNG(N))
            IF(N.EQ.3 .AND. NRNG(N).GT.0)CALL DMPFLOAT(SW,NRNG(N))
         END DO
         MDUMP=MDUMP-1 
      END IF

      IF(NAZ.LT.1)GO TO 110

C----------------------------------------------------------------
C     Fill the large data array with DZ, VE, and SW for this beam
C
      DO 100 K=1,NFLDS
c--------if(iswp.eq.1)print *,'RDNEX: k,namfld=',k,namfld(k)
         IF(NAMFLD(K).EQ.'DZ      ' .OR.
     X      NAMFLD(K).EQ.'VE      ' .OR.
     X      NAMFLD(K).EQ.'SW      ')THEN
            DO I=1,MXR
               DAT(I,NAZ,K)=BDVAL
               RDZ(I)=BDVAL
               RVE(I)=BDVAL
               RSW(I)=BDVAL
            END DO
         END IF

         IF(NAMFLD(K).EQ.'DZ      ')THEN
C           Fill the large data array with DZ 
C           by interpolating to VE gate locations
C
            IF(NGTSDZ .GT. 0)THEN
               DO 70 I=1,NGTSDZ
                  IF(DZ(I+1).NE.BADNEX .AND. DZ(I).NE.BADNEX)THEN
                     DEL=DZ(I+1)-DZ(I)
                     I1=4*(I-1)+1
                     I2=I1+4
                     DO 66 II=I1,I2
                        DAT(II,NAZ,K)=DZ(I)+0.25*DEL*(II-I1)
                        RDZ(II)=DAT(II,NAZ,K)
 66                  CONTINUE
                  END IF
 70            CONTINUE
            END IF

         ELSE IF(NAMFLD(K).EQ.'VE      ')THEN
C           Fill the large data array with VE
C
            IF(NGTSVE .GT. 0)THEN
               DO 80 I=1,NGTSVE
                  DAT(I,NAZ,K)=BDVAL
                  IF(VE(I).NE.BADNEX)DAT(I,NAZ,K)=VE(I)
                  RVE(I)=DAT(I,NAZ,K)
 80            CONTINUE
            END IF

         ELSE IF(NAMFLD(K).EQ.'SW      ')THEN
C           Fill the large data array with SW
C
            IF(NGTSVE .GT. 0)THEN
               DO 90 I=1,NGTSVE
                  DAT(I,NAZ,K)=BDVAL
                  IF(SW(I).NE.BADNEX)DAT(I,NAZ,K)=SW(I)
                  RSW(I)=DAT(I,NAZ,K)
 90            CONTINUE
            END IF

         END IF

C        Dump MDUMP beams worth of data after filling DZ to VE resolution
C
         IF(IFD.EQ.1 .AND. MDUMP.GT.0)THEN
            DO N=1,NFL_NEX
               WRITE(6,33)N,NAMNEX(N),NRNG(N),NGTS,RMIN(N),R0
               IF(N.EQ.1 .AND. NRNG(N).GT.0)CALL DMPFLOAT(RDZ,NGTS)
               IF(N.EQ.2 .AND. NRNG(N).GT.0)CALL DMPFLOAT(RVE,NGTS)
               IF(N.EQ.3 .AND. NRNG(N).GT.0)CALL DMPFLOAT(RSW,NGTS)
            END DO
            MDUMP=MDUMP-1
         END IF

 100  CONTINUE
C----------------------------------------------------------------
    
 110  CONTINUE

      IF(NAZ.GT.MXA .OR. IEOS.EQ.1 .OR. IEOV.EQ.1 .OR. IEOT.EQ.1)THEN

C        Return to main program if: (1) Maximum number of beams (MXA) 
C        exceeded or (2) End of a sweep, volume scan, or data.
C
         NANG(1)=NAZ
         IF(NAZ.GT.1)THEN
            AVGI=AZSUM/FLOAT(NAZ)
         ELSE
            AVGI=0.0
         END IF
         AZSUM=0.0
         PLTSW = .FALSE.
         VECTS = .FALSE.
         CALL LABELPR(ZSTR,COLRFIL,PLTSW,VECTS,NFRAME,LABLS,
     X        IGRPLT,BGFLAG)
         RETURN

      ELSE

C        Continue reading input data file
C
         GO TO 10
      END IF

      END
