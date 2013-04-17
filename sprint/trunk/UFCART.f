      PROGRAM UFCART
C     
C     NOTICE
C     Copyright 1995 University Corporation for Atmospheric Research
C     National Center for Atmospheric Research
C
C This software was developed by NCAR, which is operated by UCAR and sponsored 
C by the National Science Foundation.
C
C Access and use of this software shall impose the following obligations on the
C user.  The user is granted the right, without any fee or cost, to use, copy,
C modify, alter, enhance and distribute this software, and any derivative works
C thereof, and its supporting documentation for any purpose whatsoever, except
C commercial sales, provided that this entire notice appears in all copies of 
C the software, derivative works and supporting documentation.  Further, the 
C user agrees to credit UCAR/NCAR in any publications that result from the use
C of this software or in any software package that includes this software.  
C The names UCAR/NCAR, however, may not be used in any advertising or 
C publicity to endorse  or promote any products or commercial entity unless 
C specific written permission is obtained from UCAR/NCAR.  The user also 
C understands that UCAR/NCAR is not obligated to provide the user with any 
C support, consulting, training or assistance of any kind with regard to the 
C use, operation and performance of this software nor to provide the user with 
C any updates, revisions, new versions or ``bug fixes.''
C
C THIS SOFTWARE IS PROVIDED BY UCAR/NCAR ``AS IS'' AND ANY EXPRESS OR IMPLIED
C WARRANTIES, INCLUDING BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
C MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO
C EVENT SHALL UCAR/NCAR BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
C DAMAGES OR ANY DAMAGES WHATSOEVER, INCLUDING BUT NOT LIMITED TO CLAIMS
C ASSOCIATED WITH THE LOSS OF DATA OR PROFITS, WHICH MAY RESULT FROM AN ACTION
C IN CONTRACT, NEGLIGENCE OR OTHER TORTIOUS CLAIM THAT ARISES OUT OF OR IN
C CONNECTION WITH THE ACCESS, USE OR PERFORMANCE OF THIS SOFTWARE.
C**********************************************************************
C
C           SOFTWARE DEVELOPMENT BY CARL MOHR and DOUG RHOADES (CSD)
C           CONTINUED BY BILL ANDERSON, SHERRIE FREDRICK, and JAY MILLER.
C
C**********************************************************************
C   Fortran Units used by the program (1, 2, and 3):
C      Unit              ----FUNCTION----
C        1 - The radar dataset is written to fort.1 which is used 
C            specifically by Sprint.  Unformatted fortran writes to
C            this unit are done in routine WRRYDK, while reads are
C            done in routine RDRYDK.
C        2 - When angle filling is done, the resulting radar dataset
C            is written to fort.2 with calls to WRRYDK from GENAZM.
C            The radar dataset is read from fort.1 with RDRYDK.
C        3 - When filtering is done, the resulting radar dataset
C            is written to fort.3 with calls to WRRYDK from DOFILT.
C            The radar dataset is read from fort.1 with LOADBMS
C            which in turn calls RDRYDK.
C**********************************************************************
C     Interpretation of Coplane and (GRIDPPI,GRIDLLE,GRIDLLZ) flags.
C     These flags are set in CRTSET.
C
C     ICOPLANE = 0  ==>  R,A,E SCANS, INTERPOLATING TO CART OR ELEV GRID
C     ICOPLANE = 1  ==>  COPLANE SCANS, INTERPOLATING TO ANGLES IN DATA
C     ICOPLANE = 2  ==>  COPLANE SCANS, INTERPOLATING TO USER SPEC. ANGLES
C     ICOPLANE = 3  ==>  COPLANE SCANS, INTERPOLATING TO CART GRID
C                        Set in RPNCAR (FOF RP field format) or UFNCAR (UF)
C                        according to sweep mode (2) coplane or (3) RHI
C     ICOPLANE = 4  ==>  RHI SCANS, INTERPOLATING TO CART GRID
C                        Set in RPNCAR (FOF RP field format) or UFNCAR (UF)
C                        according to sweep mode (2) coplane or (3) RHI
C                        Uses COPLANE interpolation code so that azimuth
C                        in the RHIs is 90 - elevation angle.
C     ICOPLANE = 5  ==>  AIRBORNE SWEEPS, INTERPOLATING TO CART GRID
C                        Set in RADAR (DORADE format)
C     IPPI     = 0  ==>  Normal interpolations to cartesian grid
C     IPPI     = 1  ==>  XY interpolations to original elevation surfaces
C     ILLE     = 1  ==>  LonLat interpolations to original elevation surfaces
C     ILLZ     = 1  ==>  LonLat interpolations to constant height surfaces
C
C     MAIN INTERPOLATION ROUTINES:
C        TRPVOL   - ground-based PPI, RHI, and COPLANE scans to Cartesian.
C        TRPARVOL - airborne helical scans from for- or aft-looking sweeps.
C                   treated as if 360 deg scans from ground-based radar.  The
C                   "vertical axis" is in the direction the airplane is flying.
C                   These constant tilt angle surfaces are displaced along the
C                   ground-relative track (heading + drift).
C        TRPPPI   - ground-based PPI scans to constant-elevation surfaces.
C        BEAMIN   - reads beams from previously-created disk file.  Called from
C                   the above interpolation driver routines.
C        TRPD     - bilinear (4-pt) interpolator of NOF fields at a single XY
C                   location.  Called for interpolation into upper and lower
C                   planes.
C        COMBIN   - finishes vertical interpolation at a single XY-location
C                   between upper and lower planes.
C        IPKDAT   - called from main interpolation drivers after COMBIN to
C                   pack NOF (NFLDS) field values into 5-dimensional array
C                   ICOB.  
C
C**********************************************************************
C
C     MAIN OUTPUT ROUTINES FOR WRITING GRIDDED DATA TO DISK FILE:
C        CARTAP   - main driver for output of cartesian disk files.  Called
C                   from main program (UFCART) and in turn calls routines 
C                   for various parts of header generation and OUTPCK for 
C                   gridded field values.
C        OUTPCK   - Unpacks field values from ICOB, repacks these into IAXK,
C                   and calls output disk writing (CWRITE).  Also calls STRID
C                   for output statistics table.
C
C**********************************************************************
C
C        CONTROL MODULE FOR TRANSFERRING UNIVERSAL FORMAT TAPES TO
C        CARTESIAN SPACE...(NOT RELATED TO ARTESIAN SPACE OR OLY)
C
C        COMMANDS:
C           INPUT   =SPECIFY INPUT INFORMATION
C           OUTPUT  =SPECIFY OUTPUT INFORMATION
C           RESET   =SET RANGES AND TOLERANCES
C           AZIMUTH =SUBSECTION DATA ALONG AZIMUTH
C           INTERP  =SPECIFIES FIELDS TO BE INTERPOLATED
C                       AND TRANSFORMATION METHODS
C           GRID    =SPECIFIES CARTESIAN GRID FOR BACKWARD COMPATIBILITY 
C           GRIDXYZ =SPECIFIES CARTESIAN GRID
C           GRIDCPL =SPECIFIES COPLANE GRID (XY and COPLANE ANGLE)
C           GRIDPPI =SPECIFIES CONSTANT-ELEVATION GRID (XY and 
C                    ELEVATION ANGLES ARE TAKEN FROM INPUT SCANS).
C           GRIDLLE =SPECIFIES CONSTANT-ELEVATION GRID AT LonLat, not XY
C                    The input elevation angles are put into the
C                    level header to allow for unequally spaced
C                    grids in the pseudo-vertical direction.
C                    Horizontal XY(LL)-grid is equally spaced in both directions.
C           GRIDLLZ =SPECIFIES CONSTANT-HEIGHT GRID AT LonLat, not XY
C                    Horizontal XY(LL)-grid is equally spaced in both directions.
C           RADAR   =SPECIFY RADAR PROCESSOR AND COORDINATES
C           ANALYT  =SPECIFY REPLACEMENT OF INPUT FIELDS WITH ANALYTIC 
C                    FUNCTIONS.
C           FXTABLE =SPECIFIES SWEEPS TO BE (NOT TO BE) PROCESSED TO ACCOMODATE
C                    SCANS THAT ARE NOT MONOTONICALLY INCREASING, E. G. NEXRAD
C                    SCANS OF 0.5, 0.5, 1.0, 1.0, 2.3, ... DEG
C           FLTERTH =SPECIFIES IF HEIGHTS ARE COMPUTED MSL OR ABOVE FLAT EARTH.
C           FILTER  =SPECIFIES RADAR-DOMAIN FILTERING TO BE DONE BEFORE INTERP.
C           ORIGIN  =SPECIFIES RADAR and ORIGIN LAT/LON.  ALSO INVOKES READING
C                    OF INFORMATION FROM NEXRAD NETWORK FILE.
C           MACHSIZ =SPECIFIES COMPUTER WORD SIZE (64 or 32 BIT) and 
C                    BYTE-SWAPPING.
C           LATLON  =SPECIFIES THE LATITUDE AND LONGITUDE HEMISPHERES.  USED
C                    TO OVER-RIDE ANY EXTERNAL SIGN CONVENTIONS.
C           DUMP    =SPECIFIES WHETHER OR NOT INPUT BEAM HOUSEKEEPING IS TO 
C                    BE DUMPED.
C           PROCESS =PROCESS VOLUME SCAN(S) WITHIN SPECIFIED TIME PERIOD AND
C                    USING CONDITIONS SPECIFIED BY ALL OTHER COMMANDS.
C           QUIT    =TERMINATE
C
C     WORDSZ - Computer word size (either 64 or 32 set in SPRINT.INC)
C     INTSZ  - Word size used for interpolated field values (16)
C     IDIM   - First dimension of ICOB used in memory allocation for 
C              packing/unpacking INTSZ words into computer words
C              IDIM=64/WORDSZ
C     MAXWRD - Maximum number of INTSZ words contained in a computer word
C              MAXWRD=IDIM*WORDSZ/INTSZ
C     IDIM2  - Second dimension of ICOB used in memory allocation for 
C              packing/unpacking INTSZ words into computer words (4)
C     MAXFLD - Maximum number of input or output fields.
C              MAXFLD=IDIM2*MAXWRD
C     MXCRT  - Maximum number of grid points along any one axis (256)
C     MAXPLN - Maximum number of grid points in a plane (65536)
C     MAXZ   - Maximum number of interpolated levels (10)
C     MAXYZ  - Maximum number of grid points in a volume
C              MAXYZ=MAXPLN*MAXZ
C
C     ICOB(IDIM,IDIM2,MAXYZ) is used to set aside enough memory for packed
C        MAXFLD field values, each with length INITSZ.
C     Memory allocated with ICOB(IDIM,IDIM2) is (IDIMxIDIM2) 32-bit words
C        or MAXFLD 16-bit words.  
C
C     MAXEL  - Maximum number of input sweeps (scans)
C     NID    - Maximum storage needed in ID for sweep information
C     NIOB   - Maximum blocksize for writing (reading) to (from) Sprint's
C              internal disk file containing input radar data
C     MAXIN  - Maximum buffer size needed for writing (reading) to (from)
C              Sprint's internal disk file 
C     MAXLEN - Maximum (buffer size)/4
C     NKOM   - Maximum number of commands allowed
C     NRCBF  - Used to allocate memory for scratch arrays
C     MAXSKP - Maximum number of entries allowed for fixed table (FXTABLE)
C     MXCNT  - Maximum number of entries allowed in calibration file
C     IVDIM  - Used to allocate memory for scratch arrays
C
      INCLUDE 'SPRINT.INC'
      DATA LFTIM,JRH6,JRH7,JRH8,IBAD/0,64,100,1000,-32768/
      DATA DEBUG,DEBUGIT/.FALSE.,.FALSE./

c-----PARAMETER (IDIM=64/WORDSZ,MAXWRD=IDIM*WORDSZ/INTSZ)
c-----PARAMETER (NRCBF=400000,IVDIM=(NRCBF+1)/(WORDSZ/8))
c-----PARAMETER (IDIM2=4,MAXFLD=IDIM2*MAXWRD,MXCRT=256)
c-----PARAMETER (MAXPLN=65536,MAXZ=10,MAXYZ=MAXPLN*MAXZ)
c-----PARAMETER (MAXEL=150,NID=129+3*MAXEL)
c-----PARAMETER (NIOB=85000,MAXIN=8500,MAXLEN=MAXIN/4)
c-----PARAMETER (BDVAL=-32768.)

c-----PARAMETER (NKOM=22,MAXSKP=27,MXCNT=500)
      PARAMETER (NKOM=22)

      COMMON ICART(MAXPLN),ICOB(IDIM,IDIM2,MAXYZ),
     X     IBLV(IDIM,NRCBF,IDIM25),ZRTAB(MXCRT,2)
      DIMENSION XGRID(MAXPLN),YGRID(MAXPLN)

      DIMENSION ICTAB(4),TLIMITS(2,MAXFLD)
      DIMENSION XRTAB(MXCRT,2),CTDBM(MXCNT),CTDBMXH(MXCNT),
     X     CTDBMXV(MXCNT),IVREF(IVDIM,2)
      DIMENSION C3(MAXFLD),C4(MAXFLD),IFLTYP(MAXFLD),ISIDE(MAXFLD)
      DIMENSION VNYQUIST(MAXEL)

      CHARACTER*8 IFLDS(MAXFLD), OFLDS(MAXFLD),NSCTP(MAXFLD)
      CHARACTER*8 FSPACE(MAXFLD),IPROC(MAXFLD),KOMM,LSKS(NKOM)
      CHARACTER*8 KRD(10),RFNAM,FNAM,P10
      CHARACTER*8 TFIELD(2,MAXFLD)
      DATA RFNAM,FNAM/'????    ','????    '/

      COMMON/ADJUST/INFLD(MAXFLD,3),SCLIF(MAXFLD,2),NIF,IOFLD(MAXFLD,3),
     X     SCLOF(MAXFLD,2),NOF,SCLAZ,SCLRNG,SCLEL,UNSCAZ,UNSCRG,UNSCEL,
     X     LOWAZ,IZAD,IS360,MINAZ,MAXAZ,METSPC(MAXFLD),IWFLD(MAXFLD),
     X     NFLI,SCLNYQ,UNSNYQ
      COMMON /IO/KPCK(NIOB),KOUT(MAXIN),IBUF(MAXIN),NBUF(MAXLEN,4),
     X     IZ8(17),ILSTREC
      COMMON /IDBLK/ID(NID)
      COMMON /RHIS/ IRHICRS,LOWAZ2,MAXAZ2,MINAZ2,ICART2(MAXPLN),
     X     ICTAB2(4),KZV(3),IDSV
      COMMON /CFLD/NFLDS
      COMMON /CFLDC/ IFIELD(MAXFLD), INTINF(MAXFLD,3)

      CHARACTER*8 INTINF,IFIELD
      CHARACTER*3 KOMDOR
C
C     Common blocks from USRORIGIN
C
      COMMON /CINP/IUN,ISKP,ORLAT,ORLON
      COMMON /RINP/RADLAT,RADLON,RADALT
      COMMON /CINPC/ NMRAD,ITAP,TAPE_UNIT
      CHARACTER*4 NMRAD
      CHARACTER*8 ITAP
      CHARACTER*8 TAPE_UNIT
      REAL RADLAT,RADLON,RADALT,ORLAT,ORLON
C
C     COMMON block variables returned from LAT_LON
C
      COMMON /HEMISPHERE/LATSPHERE,LONSPHERE,LAT_SIGN,LON_SIGN
      CHARACTER*8 LATSPHERE,LONSPHERE
      REAL LAT_SIGN,LON_SIGN
      DATA LATSPHERE,LONSPHERE/'NORTH','WEST'/
      DATA LAT_SIGN,LON_SIGN/+1.0,+1.0/

      COMMON /UNITS/LUN,LSKP,FEETS
      COMMON /UNITSC/ IPROJ,LTAP,ISCI
      CHARACTER*4 IPROJ
      CHARACTER*8 LTAP,ISCI

      COMMON /FXTABL/ IFXTAB,ISKIP,IACCPT,FTABLE(MAXSKP),ITRAN
      COMMON /CRNG/ELTUS,MNBEM,RUSR1,RUSR2,RG1,RG2,DRG,NRG,VNYQ,
     X     RNOTUS,DRGUS,CFAC1,CFAC2,CFAC3
      COMMON /CRNGC/ IRCFXL
      CHARACTER*8 IRCFXL

      COMMON /CPRO/KDAY,KBTIM,KETIM,IROV,ISWMTH,FXSTOL
      COMMON /CPROC/ IREORD(3)
      CHARACTER*1 IREORD

      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X     ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      COMMON /SETAZM/ USAZ1,USAZ2,USGAP,USINC,IFOR36,IFORDR
      COMMON /AZSUB/ AT1,AT2,IFLAG,IFLADJ
      COMMON /FORMAT/ IRP,IBLOCK
      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI,ILLE,ILLZ

      COMMON /BYTORD/ MBYTE,SWAPPING
      INTEGER SWAPPING
      INTEGER USER_DEF_ORIGIN,RADAR_CHOSEN
      INTEGER BEGINNING
      INTEGER FNUM
 
      DATA NCARD,DASANG/0,-999.0/
 
      DATA IPR/6/
      DATA LSKS /'INPUT   ','OUTPUT  ','RESET   ','AZIMUTH ',
     X           'INTERP  ','GRID    ','PROCESS ','QUIT    ',
     X           'GRIDXYZ ','GRIDCPL ','GRIDPPI ','GRIDLLE ',
     X           'GRIDLLZ ','RADAR   ','ANALYT  ','FXTABLE ',
     X           'FLTERTH ','FILTER  ','ORIGIN  ','MACHSIZ ',
     X           'LATLON  ','DUMP    '/
      DATA IFLAT/0/
      DATA IFD,IFD_RAYS/0,0/

      DIMENSION IUNPK(MAXFLD)

      CHARACTER*8 HHMMSS
      character*8 tyme
      integer begsec,endsec,totsec,totmin,remsec
C
C     OUTPUT VERSION AND COPYRIGHT INFO
C
      CALL VERSOUT

      call clock(tyme)
      read (tyme,11)ihr,imn,isc
 11   format(i2,1x,i2,1x,i2)
      begsec = isc + 60*imn + 3600*ihr
      print *,' '
      print *,'++++++++++++++++++++++++++++++++++++'
      print *,'Sprint execution started at ',tyme
      print *,'++++++++++++++++++++++++++++++++++++'
      print *,' '
      print *,'UFCART-IBLV array: idim,nrcbf,idim25=',idim,nrcbf,idim25
      print *,'UFCART-IBLV array: idim*nrcbf*idim25=',idim*nrcbf*idim25
      print *,' '
      ISKP=0
      IFLAG=0
      AT1=0.0
      AT2=0.0
      IFLADJ=0
      AZCOR=0.0
      IAZFLG=0
      FNUM=0
      NUMFILT=0
      SWAPPING = 0
      NMRAD = '    '
      USER_DEF_ORIGIN = 0
      RADAR_CHOSEN = 0
      BEGINNING    = 0
C
C     GET BYTE ORDERING USED BY MACHINE SPRINT IS RUNNING ON
C
      print *,'UFCART-befor CBYTE: byte order =',mbyte
      CALL CBYTE(MBYTE)
      print *,'UFCART-after CBYTE: byte order =',mbyte
      print *,'UFCART-befor INITAL: icrtst,inptst =',icrtst,inptst
      CALL INITAL(ICRTST,INPTST)
      print *,'UFCART-after INITAL: icrtst,inptst =',icrtst,inptst
C
C        READ NEXT COMMAND
C
 100  CONTINUE
      CALL KARDIN(KRD)
      NCARD=NCARD+1
      READ (KRD,102)KOMM
 102  FORMAT (A7)
      IGO=LOCATEC(KOMM,LSKS,NKOM)
      IF (IGO.LT.1 .OR. IGO.GT.NKOM) GOTO 120
      GOTO (150,160,170,180,
     X      200,210,220,900,
     X      210,210,210,210,
     X      210,240,250,260,
     X      270,280,290,310,
     X      320,330),IGO
 120  CONTINUE
      PRINT 103,KOMM
 103  FORMAT (5X,'***UNRECOGNIZABLE COMMAND -- ',A3)
      STOP

C     INPUT --> 150
C
 150  CONTINUE
C
C     IROV= (-1) APPEND, (0) NORMAL PROCESSING, (1) RUNOVER
C
      CALL INPFIL(KRD,INPTST,IAZFLG,AZCOR,USER_DEF_ORIGIN,
     X            RADAR_CHOSEN,IROV)
      GOTO 100

C     OUTPUT --> 160
C
 160  CONTINUE
      CALL OUTFIL(KRD)
      GOTO 100

C     RESET --> 170
C
 170  CONTINUE
      CALL RNGFIL(KRD)
      GOTO 100

C     AZIMUTH --> 180
C
 180  CONTINUE
      CALL AZMFIL(KRD,AZCOR,IAZFLG,DASANG)
      GOTO 100

C     INTERP --> 200
C
 200  CONTINUE
      CALL INTERP(KRD,TFIELD,TLIMITS,NTHRSH,ISIDE)
      GOTO 100

C     GRID, GRIDXYZ, GRIDCPL, GRIDPPI, GRIDLLE, or GRIDLLZ --> 210
C
 210  CONTINUE
      CALL CRTSET(KRD,MAXPLN,MXCRT,ICRTST,MAXYZ)
C
C     REMEMBER ORIG. X,Y, AND Z GRID LAYOUT FOR USE WITH RHI SCANS
C
      NDX=NX
      NDY=NY
      NDZ=NZ
      GOTO 100

C     PROCESS --> 220
C
 220  CONTINUE
      CALL PROFIL(KRD)
      NUFST=0
      IVOL =0
      IF (ISKP.GT.0 .AND. IRP.NE.2) THEN
C
C     SKIP VOLUMES ON INPUT UNIT BEFORE PROCESSING
C
         IF (IRP.EQ.1) ISKP=ISKP+1
         CALL SKPVOL(IUN,ISKP)
         ISKP=0
      ELSE IF (ISKP.GT.0 .AND. IRP.EQ.2) THEN
         WRITE(*,*)'***SKIPPING DORADE VOLUMES NOT YET ENABLED***'
      END IF
 225  CONTINUE
C
C     I HAVE NO IDEA WHY THE FOLLOWING TWO LINES ARE IN HERE. WDA
C
      IBLV(1,10300,1)=-997
      IBLV(2,10300,1)=-997

      IF(RADAR_CHOSEN .EQ. 0) THEN
         PRINT *,"                       "
         PRINT *,'PLEASE ENTER A RADAR NUMBER IN THE INPUT',
     X       ' COMMAND OR A FOUR LETTER RADAR NAME IN THE',
     X       ' ORIGIN COMMAND'
         STOP
      END IF

C     Open scratch unit used for debugging printout
C
      call clock(hhmmss)
      write(7,*)'Open scratch unit #7 at ',hhmmss
      open(unit=7,access='sequential',status='old')
      write(9,*)'Open scratch unit #9 at ',hhmmss
      open(unit=9,access='sequential',status='old')

      IF (IRP.EQ.0) THEN
         CALL UFNCAR(NUFST,IBLV,ICOB,ICRTST,NTHRSH,
     X               TFIELD,TLIMITS,RFNAM,FNUM,P1,P2,P3,P4,P10,AZCOR,
     X               DASANG,ISIDE,USER_DEF_ORIGIN,RADAR_CHOSEN,
     X               VNYQUIST,IFD,IFD_RAYS)
      ELSE IF (IRP.EQ.1) THEN
         CALL RPNCAR(NUFST,IBLV,ICOB,ICRTST,CTDBM,CTDBMXH,CTDBMXV,
     X               NTHRSH,TFIELD,TLIMITS,RFNAM,FNUM,P1,P2,P3,P4,P10,
     X               AZCOR,DASANG,ISIDE,USER_DEF_ORIGIN,RADAR_CHOSEN,
     X               VNYQUIST,IFD,IFD_RAYS)
      ELSE IF (IRP.EQ.2) THEN
         write(9,*)'Before DORVOL: kday,ifd,ifd_rays=',kday,ifd,ifd_rays
         write(9,*)'Before DORVOL: hdtrptr=',
     X        'heading  drift  track   roll  pitch   tilt rotang'
         write(9,*)'Before DORVOL: fae=  fxang   azim   elev'
         CALL DORVOL(NUFST,IBLV,ICOB,ICRTST,CTDBM,CTDBMXH,CTDBMXV,
     X               NTHRSH,TFIELD,TLIMITS,RFNAM,FNUM,P1,P2,P3,P4,P10,
     X               AZCOR,DASANG,ISIDE,USER_DEF_ORIGIN,RADAR_CHOSEN,
     X               KOMDOR,VNYQUIST,IFD,IFD_RAYS)
      ELSE IF (IRP.EQ.3) THEN
         CALL NEXVOL(NUFST,IBLV,ICOB,ICRTST,CTDBM,CTDBMXH,CTDBMXV,
     X               NTHRSH,TFIELD,TLIMITS,RFNAM,FNUM,P1,P2,P3,P4,P10,
     X               AZCOR,DASANG,ISIDE,BEGINNING,USER_DEF_ORIGIN,
     X               RADAR_CHOSEN,VNYQUIST,IFD,IFD_RAYS,IVOL)
      ELSE
         WRITE(*,*)'***INVALID DATA FORMAT IN UFCART***'
         STOP
      END IF
c-----debug (ljm)
      print *,'After data ingest: #input levels [id(35)]=',id(35)
      print *,'Nyquist velocities:'
      CALL DMPFLOAT(VNYQUIST,ID(35))
c-----debug (ljm)
      CALL FLUSH(6)

      IF ((NUFST.EQ. 1) .OR. (NUFST .EQ. 2)) GOTO 228
      CALL METHOD(IPR)
      CALL INHSUM(IPR,'-','Y',AZCOR,DASANG)

C     Open scratch unit used for debugging printout
C
      call clock(hhmmss)
      write(8,*)'Open scratch unit #8 at ',hhmmss
      open(unit=8,access='sequential',status='old')

C     ICOPLANE = 0  ==>  SECT or SURV scans to (X,Y,Z) or (X,Y,E)
C     ICOPLANE = 1  ==>  Coplane scans to original data coplane angles
C     ICOPLANE = 2  ==>  Coplane scans to user-specified coplane angles
C     ICOPLANE = 3  ==>  Coplane scans to (X,Y,Z).  Set in RPNCAR (FOF 
C                        RP field format) or UFNCAR (UF) according to 
C                        sweep mode (2) coplane or (3) RHI
C     ICOPLANE = 4  ==>  RHI scans to (X,Y,Z). Set in RPNCAR (FOF RP 
C                        field format) or UFNCAR (UF) according to sweep 
C                        mode (2) coplane or (3) RHI.  Uses COPLANE 
C                        interpolation code so that azimuth in the RHIs 
C                        is 90 deg - elevation angle.
C     ICOPLANE = 5  ==>  Airborne sweeps to (X,Y,Z).  Set in RADAR (DORADE 
C                        format).
C
      write(8,*) 'UFCART - before CRTOUT:    icoplane=',icoplane
      write(8,*) 'UFCART - before CRTOUT: ppi-lle-llz=',ippi,ille,illz

c     CALL CRTOUT(ICART,ICTAB,ICRTST,INPTST,IOP,XGRID,YGRID)
c
      IF (ICOPLANE.EQ.0 .OR. ICOPLANE.EQ.3) THEN
         CALL CRTOUT(ICART,ICTAB,ICRTST,INPTST,0,XGRID,YGRID)
      ELSE IF (ICOPLANE.EQ.1 .OR. ICOPLANE.EQ.2) THEN
         CALL CRTTCP(ICART,ICTAB,ICRTST,INPTST)
      ELSE IF (ICOPLANE.EQ.4) THEN
         CALL CRTOUT(ICART2,ICTAB2,ICRTST,INPTST,1,XGRID,YGRID)
         CALL CRTOUT(ICART,ICTAB,ICRTST,INPTST,2,XGRID,YGRID)
      ELSE IF (ICOPLANE.EQ.5) THEN
         CALL CRTOUT(ICART,ICTAB,ICRTST,INPTST,3,XGRID,YGRID)
      END IF
      CALL FLUSH(6)
C
C     DO FILTERING
C
      IF (NUMFILT.GT.0) THEN
c--------debug (ljm)
         print *,'Before filtering: id(35),vnyq=',id(35),
     +        (vnyquist(n),n=1,id(35))
         call flush(6)
c--------debug (ljm)
         CALL DOFILT(IBLV,IFLDS,OFLDS,IFLTYP,IPROC,FSPACE,C3,C4,
     X        NSCTP,NUMFILT,ICOPLANE,VNYQUIST)
      END IF
C
C     ---------------------------------
C     CALL MAIN INTERPOLATION ROUTINES:
C     ---------------------------------
C
C        TRPVOL   - ground-based PPI, RHI, and COPLANE scans to Cartesian.
C        TRPARVOL - airborne helical scans from for- or aft-looking sweeps.
C                   treated as if 360 deg scans from ground-based radar.  The
C                   "vertical axis" is in the direction the airplane is flying.
C                   These constant tilt angle surfaces are displaced along the
C                   ground-relative track (heading + drift).
C        TRPPPI   - ground-based PPI scans to constant-elevation surfaces.
C
      IF (ICOPLANE.NE.5 .AND. IPPI.EQ.0) THEN
C----------------------------------------------------------------------------
C     ICOPLANE = 0-4 ==> All interps except airborne and (X,Y,E) or (L,L,E)
C----------------------------------------------------------------------------
         CALL TRPVOL(ICART,ICTAB,ICOB,NDX,NDY,NDZ,ZRTAB,
     X        IBLV,NST,DASANG,IFLAT,NUMFILT,XRTAB,
     X        IVREF,VNYQUIST,XGRID,YGRID)
         IF (NST.NE.0) GOTO 227

      ELSE IF (ICOPLANE.EQ.5) THEN
C----------------------------------------------------------------------------
C     ICOPLANE = 5  ==>  Interpolation of airborne sweeps to (X,Y,Z)
C----------------------------------------------------------------------------
         CALL TRPARVOL(ICART,ICTAB,ICOB,NDX,NDY,NDZ,ZRTAB,
     X        IBLV,NST,DASANG,IFLAT,NUMFILT,XRTAB,IVREF)

      ELSE IF (IPPI.EQ.1) THEN
C----------------------------------------------------------------------------
C     IPPI,ILLE= 1  ==>  XY or LonLat interps to constant elevation surfaces
C----------------------------------------------------------------------------
         CALL TRPPPI(ICART,ICTAB,ICOB,NDX,NDY,NDZ,ZRTAB,
     X        IBLV,NST,DASANG,IFLAT,NUMFILT,XRTAB,
     X        IVREF,VNYQUIST,XGRID,YGRID)

c-----debug (ljm)
c         i = 1
c         j = 1
c         k = 1
c         nmax=ndx*ndy*ndz
c         print *,'Ufcart-after trppi: ',idim,ndx,ndy,ndz,nmax
c         do n=1,nmax
c            if(mod(k-1, 2).eq.0 .and. 
c     +           mod(j-1,10).eq.0 .and. 
c     +           mod(i-1,10).eq.0)then
c               call gbytes (icob(1,1,n),iunpk,0,16,0,8)
c               write(*,1769)n,k,j,i,(iunpk(nf),nf=1,nflds)
c 1769          format('Ufcart: n=',i12,' k,j,i=',3i4,' iunpk=',16i8)
c            end if
c            i=i+1
c            if(i.gt.ndx)then
c               i=1
c               j=j+1
c            end if
c            if(j.gt.ndy)then
c               j=1
c               k=k+1
c            end if
c            if(k.gt.ndz)then
c               k=1
c            end if
c         end do
c-----debug (ljm)

      END IF
      CALL FLUSH(6)

      IF (ICOPLANE.EQ.4) THEN
C
C     IF RHI, SWITCH BACK AXES
C
         NT=NX
         NX=NZ
         NZ=NY
         NY=NT

         T1=X1
         T2=X2
         TD=XD

         X1=Z1
         X2=Z2
         XD=ZD
         
         Z1=Y1
         Z2=Y2
         ZD=YD

         Y1=T1
         Y2=T2
         YD=TD

         TORG=XORG
         XORG=ZRAD
         ZRAD=YORG
         YORG=TORG
      END IF

      CALL CARTAP(IPR,IREORD,ICOB,NX,NY,NZ,IBLV(1,1,1),IBLV(1,1,2),NST,
     X            DASANG,VNYQ,VNYQUIST,MAXEL)

      IF (NST.NE.0) STOP 7701
 227  CONTINUE
      IF(NUFST .EQ. 3) THEN
         IF (KOMDOR .EQ. 'QUI')THEN
            GO TO 900
         ELSE
            BEGINNING = 0
            GO TO 100
         END IF
      END IF

      IF(NUFST .EQ. 4) THEN
C We have reached the end of sweep files to be read in.  
         IF (KOMDOR .EQ. 'QUI')THEN
            GO TO 900
         ELSE
            BEGINNING = 0
            GO TO 100
         END IF
      END IF  

      IF (NUFST.EQ.0) THEN
         GO TO 225
      END IF
 228  CONTINUE
      GOTO 100

C     RADAR --> 240
C
 240  CONTINUE
      CALL RADAR(KRD,CTDBM,CTDBMXH,CTDBMXV)
      GOTO 100

C     ANALYT --> 250
C
 250  CONTINUE
      CALL ANALYT(KRD,RFNAM,FNAM,FNUM,P1,P2,P3,P4,P10)
      GOTO 100

C     FXTABLE --> 260
C
 260  CONTINUE
      CALL FXTABLE(KRD)
      GOTO 100

C     FLTERTH --> 270
C
 270  CONTINUE
      CALL FLAT(KRD,IFLAT)
      GOTO 100

C     FILTER --> 280
C
 280  CONTINUE
      CALL FILTER(KRD,IFLDS,OFLDS,IFLTYP,IPROC,FSPACE,C3,C4,NSCTP,
     X            NUMFILT,IFLTALL)
      GOTO 100

C     ORIGIN --> 290
C
 290  CONTINUE
      CALL USRORIGIN(KRD,RADAR_CHOSEN,ANGXAX)
      USER_DEF_ORIGIN = 1
      GOTO 100

C     MACHSIZ --> 310
C
 310  CALL MACHSIZ(KRD,MBYTE,SWAPPING)
      GOTO 100

C     LATLON --> 320
C
 320  CALL LAT_LON(KRD,LATSPHERE,LONSPHERE,LAT_SIGN,LON_SIGN)
      GOTO 100

C     DUMP --> 330
C
 330  CALL DUMP(KRD,IFD,IFD_RAYS)
      GOTO 100

C     QUIT --> 900
C
 900  CONTINUE
      PRINT 901
 901  FORMAT (5X,'---QUIT CARD ENCOUNTERED---')
      IF(IRP.EQ.3) THEN
         CALL NEXRAD_CLOSE_FILE(TAPE_UNIT,IUN)
      ELSE
        CALL CSLEEP()
        CALL CCLOSE
      END IF

C     Close scratch units used for debugging printout
C
      CLOSE(UNIT=7)
      CLOSE(UNIT=8)
      CLOSE(UNIT=9)

      call clock(tyme)
      read (tyme,11)ihr,imn,isc
      endsec = isc + 60*imn + 3600*ihr
      totsec = endsec - begsec
      totmin = int(float(totsec)/60.0)
      remsec= totsec - 60*totmin
      print *,' '
      write(6,903)
 903  format(72('+'))
      write(6,905)tyme,totsec,totmin,remsec
 905  format('Sprint execution ended at ',a8, 
     X     ' taking ',i6,' seconds or MM:SS=',i3.2,':',i2.2)
      write(6,903)

      STOP
      END
