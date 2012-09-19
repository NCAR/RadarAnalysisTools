      PROGRAM CEDRIC
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

C Comment #1:
C There are remnants throughout the CEDRIC code that were relevant to running
C CEDRIC on the older CRAY computers.  Any references to TAPE are no longer
C valid.  A single DUMMY.f routine was added years ago to accomodate some
C routines that were unique to the CRAY.  These were UPDATE_COS, IOWAIT,
C DATAIN, OUTPCK, and RDTAPE.  If any of these routines are called, that
C routine inside DUMMY.f will immediately return to the calling routine 
C with no action.  Calls to routines now within DUMMY.f were never eliminated
C over the years.  DUMMY is there only to prevent compilation errors.
C
C Comment #2:
C All MDV-related routines have been copied into the MDVstuff directory and
C are no longer a part of the current archived version of CEDRIC.  Routines
C in MDVstuff that are still in the main CEDRIC directory are compiled since
C some parts are still being called elsewhere (non-MDV).  See the makefiles
C list. 
C
C Comments #1 and #2 added by LJM on Sep 18, 2012.

C     
C     PARAMETERIZATION SET FOR (511 X 511) GRID
C     WITH 12 PLANES AVAILABLE.
C     
C     PARAMETER (MAXLCM=2000000)

      INCLUDE 'CEDRIC.INC'
      PARAMETER (MAXBSZ=(MXCRT-2)*MAXPLN)
      PARAMETER (MXBSYN=MXCRT*MAXPLN)
      PARAMETER (MXFLSH=MAXPLN*2)
      PARAMETER (MAXOPT=56)
      PARAMETER (NUN=6,NUT=5)
      PARAMETER (MXL=20000)
      COMMON LCMB(1),MEMUSE
      COMMON /FMTTYPE/ WRFFLG
      INTEGER WRFFLG
      DIMENSION IBUF(MAXPLN,MXCRT+27)
      COMMON /AIRTRCK/ XACT(MXL),YACT(MXL),ZACT(MXL),BEGACT,DELACT,
     X                 NACT,DTAC,UACT(MXL),VACT(MXL),WACT(MXL),IACTWND,
     X                 AIRTHK

C     Common blocks for NLDN and LMA datasets
C        MXNL   - NLD CG lightning strikes (100000)
C        MXLM   - LMA lightning channel (1000000)
C
      PARAMETER (MXNL=100000,MXLM=1000000)
      COMMON/NLDN/XNLD(MXNL),YNLD(MXNL),TNLD(MXNL),PNLD(MXNL),
     X     INLD,DTNLD,DZNLD,THKNLD

      DOUBLE PRECISION TLMA
      COMMON/LMA/XLMA(MXLM),YLMA(MXLM),ZLMA(MXLM),TLMA(MXLM),
     X     ILMA,DTLMA,DZLMA

C     ARRAYS/VARIABLES FOR STATION PLOTTING
      PARAMETER(MXK=1000,MXNET=20)

      COMMON /STALOC/ XSTA(MXK),YSTA(MXK),ZSTA(MXK),IMRK
      DIMENSION NNET(MXNET)
      CHARACTER*6 SMRK(MXNET)
      CHARACTER*7 NMRK(MXK)
C
C     COMMON block variables returned from LAT_LON or uses defaults.
C
      COMMON /HEMISPHERE/LATSPHERE,LONSPHERE,LAT_SIGN,LON_SIGN,LATLON
      CHARACTER*8 LATSPHERE,LONSPHERE
      REAL LAT_SIGN,LON_SIGN
      LOGICAL LATLON
      DATA LATSPHERE,LONSPHERE/'NORTH','WEST'/
      DATA LAT_SIGN,LON_SIGN/+1.0,+1.0/
      DATA IBAD/-32768/

      INTEGER LATLON_KARD,ORIGINAL_LAT(3),ORIGINAL_LON(3)

      COMMON /UNITS/ LIN,LOUT,LPR,LSPOOL
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      CHARACTER*4 AXNAM
C      DATA AXNAM /'X','Y','Z'/
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      COMMON /GUI/ IFATAL,IGUISTAT
      COMMON /CDFNET/ ICDFID(MXCDF),IDIMDAT(4,MXCDF),
     X     IDIMID(1,MXCDF),IVAR(NFMAX+1,MXCDF),IFILE,ISYNFLG,
     X     ICDUNT(MXCDF),IUSWRP
      CHARACTER*5 LISTOP(MAXOPT),ICOM
      CHARACTER*3 CTEMP1
      CHARACTER*4 CTEMP
      CHARACTER*8 KRD(10),GFIELD(NFMAX)
      CHARACTER*8 QMARK
      DATA QMARK/'Unknown?'/
      DATA LIN,LOUT,LPR,LSPOOL,IDFLAS,LUFLAS/0,0,6,7,3,3/

C     LISTOP - List of CEDRIC commands
C     MAXOPT - Maximum number of CEDRIC commands
C
C     NOTE: Update CEDRIC documentation since all of
C           these may not be documented (LJM 8/29/2012).
C
      DATA LISTOP/'BAD  ','CHANG','CODED','COMME','CONVE','CREAT',
     X            'DIGIT','DELET','FIELD','FILTE','FIXID','FUNCT',
     X            'HISTO','INTEG','PATCH','GRAPH','READV','RENAM',
     X            'REMAP','SAMPL','SHIFT','STATS','SYNTH','THREE',
     X            'TRANS','UNFOL','WINDO','WRITV','CROSS','SURFA',
     X            'REGRE','LAPLA','WTRAN','STPLO','RELAX','MASS2',
     X            'COORD','FLTER','READA','READS','GUI  ','BCKGR',
     X            'LATLO','VAD  ','PLTVA','GETSN','GETNL','GETLM',
     X            'GETVA','PINTR','HINTR','DATSN','PLTCO','SETLI',
     X            'END  ','QUIT '/
      DATA ICORD/0/
      DATA IFLAT/0/
      character*8 tyme
      integer begsec,endsec,totsec,totmin,remsec

C     VAD analysis and plotting arrays
C        NVDMX  - VAD-analyzed fields (6)
C        MXVD   - Number of individual gridded levels allowed 
C                 within a single volume dataset (MXVD=MAXZLEV).
C
      PARAMETER (NVDMX=6,MXVD=51)
      CHARACTER*8 NAMFLD(NVDMX),NAMPLVD(MXVD)
      CHARACTER*4 TYPVD(NVDMX,MXVD),VADTYPE(MXVD)
      DIMENSION XMNVD(NVDMX,MXVD),XMXVD(NVDMX,MXVD)
      DIMENSION XSCLVD(NVDMX,MXVD),XREFVD(NVDMX,MXVD)
      DIMENSION ZMNVD(MXVD),ZMXVD(MXVD),ISKPVD(MXVD)
      DIMENSION U_VD(MXVD),V_VD(MXVD),AZMVD(MXVD),WFILT(MXVD)
      CHARACTER*2 NAMINF(4),NAMOUF(4),NAMDBZ(4)

c     Initialize some variables
c
      do i=1,nfmax
         gfield(i)=qmark
      end do
c      iend=maxpln
c      jend=mxcrt+27
c      do j=1,jend
c         do i=1,iend
c            ibuf(i,j)=ibad
c         enddo
c      enddo

 1    CONTINUE
      
C     
C     INTIALIZE EDIT FILE CHARACTERISTICS (RAM AND/OR DISK FILE)
C     
      CALL CGETMEM(LCMB,MEMUSE)
C     
C     Initialize GKS, OPNGKS will use unit 2 for its output
C     
      CALL OPNGKS

C     THE FOLLOWING LINE IS FOR GFLAS ROUTINES
C     CALL GFLAS1(IB) - Initiates storage of plotting instructions into 
C                       a disk file (IB), where IB = 0 - 99 is the storage 
C                       buffer number, e.g. GNFB03.
C     CALL GFLAS2     - Terminates storing plotting instructions into the
C                       open disk file (buffer IB).  Any future plotting 
C                       instructions will now go into normal output (gmeta).
C     CALL GFLAS3(IB) - Inserts plotting instructions into gmeta file that 
C                       were stored in buffer # IB.
C        GFLAS1(1) - Vectors (VECDSP).
C        GFLAS1(2) - Sounding Skew-T background (GETSND).
C        GFLAS1(3) - Aircraft track (READAIR)
C        GFLAS1(4) - Mesonet stations (PLTSTA)
C        GFLAS1(5) - NLDN cloud-to-ground lightning (PLTNLD)
C        GFLAS1(6) - LMA incloud lightning (PLTLMA - to be implemented)
C        GFLAS3(*) - Called in plotting routine and/or PLOTCH.
C
C     CALL GOPWK(ID,IC,3)
C          ID - any nonngegative integer, except 2, 5, or 6.
C          IC - valid Fortran logical unit
C           3 - workstation independent storage segment (WISS)
C     Routine CEDERX - CALL CLSGKS to close workstation
C                      CALL GCLWK(IDFLAS) to close all GFLAS buffers
C
      CALL GOPWK(IDFLAS,LUFLAS,3)
      CALL GSCLIP(0)
      CALL DFCLRS(0)
C      CALL PCSETI('CD',0)
      CALL PCSETC('FC','&')

C     Get default line thickness (ILW) and reset to JLW.
C     Restore line thickness before leaving routine.
C
      CALL GETUSV('LW',ILW)
      JLW=1200
      IF(JLW.LT.ILW)JLW=ILW
      CALL SETUSV('LW',JLW)
      print *,'Default line thickness ',ilw,' reset to ',jlw
      print *,'Computer word size, maxbuf =',wordsz, maxbuf
      print *,'Maximum number of fields   =',nfmax
      print *,'Maximum numbers of x and y =',maxx,maxy
      print *,'Maximum number of z-levels =',mxcrt
      
      IPR=LPR
C      REWIND LSPOOL
      NFL=0
C     
C     INITIALIZE THE SYMBOL TABLE- GENERATE LOWER CASE LETTERS
C     TEMPORARY SITUATION BECAUSE OF COMMUMNICATIONS
C     PROTOCOLS.
C     
      CALL SYMINT
      CALL VERSOUT

      call clock(tyme)
      read (tyme,11)ihr,imn,isc
 11   format(i2,1x,i2,1x,i2)
      begsec = isc + 60*imn + 3600*ihr
      print *,' '
      print *,'++++++++++++++++++++++++++++++++++++'
      print *,'Cedric execution started at ',tyme
      print *,'++++++++++++++++++++++++++++++++++++'
      print *,' '
C     
C
C     INITIALIZE AXIS LABELS AND SCALE FACTORS
C
      IUNAXS=1
      WRITE(CTEMP1,33)LABAXS(3,1)
 33   FORMAT(A3)

      DO I = 1,3
         AXNAM(I) = '    '
      ENDDO

      CTEMP='KM'
      READ(CTEMP,13)LABAXS(1,1)
 13   FORMAT(A2)
      READ(CTEMP,13)LABAXS(2,1)
      IF (CTEMP1.NE.'DEG') READ(CTEMP,13)LABAXS(3,1)
      CTEMP='NM'
      READ(CTEMP1,13)LABAXS(1,2)
      READ(CTEMP1,13)LABAXS(2,2)
      CTEMP='K-FT'
      READ(CTEMP,17)LABAXS(3,2)
 17   FORMAT(A4)
      CTEMP='MI'
      READ(CTEMP,13)LABAXS(1,3)
      READ(CTEMP,13)LABAXS(2,3)
      CTEMP='K-FT'
      READ(CTEMP,17)LABAXS(3,3)


      LATLON_KARD = 0
      SCLAXS(1,1)=1.0
      SCLAXS(2,1)=1.0
      SCLAXS(3,1)=1.0
      SCLAXS(1,2)=0.5399568035
      SCLAXS(2,2)=0.5399568035
      SCLAXS(3,2)=3.280833
      SCLAXS(1,3)=0.6213688756
      SCLAXS(2,3)=0.6213688756
      SCLAXS(3,3)=3.280833
      IUSWRP = 0

C     INITIALIZE C VARIABLES STRUCTURE USED FOR WRITING OUT NETCDF
      CALL icstruct()

C     
C     READ IN THE NEXT COMMAND TO PROCESS
C     
 5    CONTINUE
      CALL KARDIN(KRD)
      READ (KRD,101)ICOM
 101  FORMAT(A5)
      IOPT=LOCATE(ICOM,LISTOP,MAXOPT)
      IF(IOPT.EQ.0) THEN
         CALL CEDERX(504,1)
         GOTO 5
      END IF
C     
C     TEST STATUS OF THE CURRENT EDIT FILE
C     
      IF(NFL.GT.0) GO TO 8
C     
C     NO EDIT FILE EXISTS. THE FOLLOWING COMMANDS ARE O.K. TO INVOKE:
C     BADVAL,COMMENT,CREATE,READVOL,SYNTHES.
C     
      IF(IOPT.EQ. 1.OR.IOPT.EQ. 4.OR.IOPT.EQ. 6.OR.IOPT.EQ.17.OR.
     X     IOPT.EQ.23 .OR. IOPT.EQ.37 .OR. IOPT.EQ.41 .OR.
     X     IOPT.EQ.49 .OR. IOPT.EQ.50 .OR. IOPT.EQ.51) GO TO 8
      CALL CEDERX(554,1)
      GOTO 5
 8    CONTINUE
C     
C     COMMAND ACCEPTED- BRANCH TO APPROPRIATE PROCEDURE
C     
      GO TO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     X      110,120,130,140,150,160,170,180,190,200,
     X      210,220,230,240,250,260,270,280,290,300,
     X      310,320,330,340,350,360,370,380,390,400,
     X      410,420,430,440,450,460,470,480,490,500,
     X      510,520,530,540,
     X      5,900), IOPT
 10   CONTINUE
C     
C     SET BAD DATA VALUE
C     
      CALL SETBAD(IPR,KRD)
      GO TO 5
 20   CONTINUE
C     
C     CHANGE INDIVIDUAL VALUES IN A FIELD
C     
      CALL CHANGE(KRD,IBUF(1,1),IBUF(1,2),IPR,NST)
      GO TO 5
 30   CONTINUE
C     
C     GENERATE CODED DISPLAYS
C     
      CALL DSPCOD(KRD,IBUF(1,1),IBUF(1,2),IPR)
      GO TO 5
 40   CONTINUE
C     
C     SEND USER SUPPLIED COMMENTS TO PRINT FILE
C     
      CALL COMMNT(IPR,KRD)
      GO TO 5
 50   CONTINUE
C     
C     CONVERGENCE CALCULATION
C     
      CALL INTGRT(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,3),IPR,NST,
     X            ICORD)
      GO TO 5
 60   CONTINUE
C     
C     USER DEFINED CARTESIAN COORDINATE SYSTEM FOR REMAPPING
C     
      CALL CREATE(IPR,KRD)
      GO TO 5
 70   CONTINUE
C     
C     GENERATE DIGITAL DISPLAYS
C     
      CALL DIGITP(KRD,IBUF(1,1),IBUF(1,2),IPR)
      GO TO 5
 80   CONTINUE
C     
C     DELETION OF EDIT FIELDS
C     
      CALL TRANSF(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,3),
     X            IBUF(1,5),IPR,NST,ICORD)
      GO TO 5
 90   CONTINUE
C     
C     SET FIELDS FOR DISPLAY
C     
      CALL WINDFD(KRD,0)
      GO TO 5
 100  CONTINUE
C     
C     FILTERING PROCEDURES
C     
      CALL FILTER(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,3),IBUF(1,15),
     X            MAXBSZ,IPR,NST)
      GO TO 5
 110  CONTINUE
C     
C     ALTER HOUSEKEEPING INFORMATION IN THE EDIT FILE HEADER
C     
      CALL FIXIDS(IPR,KRD)
      GO TO 5
 120  CONTINUE
C     
C     FUNCTION- ALGEBRAIC MANIPULATION OF DATA FIELDS
C     
      MAXPXY=MAXBSZ/NPLANE
      CALL FUNCTN(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,3),NPLANE,
     X            MAXPXY,IPR,NST,IFLAT,LATLON)
      GO TO 5
 130  CONTINUE
C     
C     HISTOGRAM DISPLAYS
C     
      CALL STATS(KRD,IBUF(1,1),IBUF(1,2),IPR)
      GO TO 5
 140  CONTINUE
C     
C     INTEGRATE- VERTICAL INTEGRATION OF DATA FIELDS
C     
      CALL INTGRT(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,3),IPR,NST,
     X            ICORD)
      GO TO 5
 150  CONTINUE
C     
C     DATA FILL AND DECIMATION PROCEDURES
C     
      CALL PATCHR(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,3),IBUF(1,15),
     X            MAXBSZ,IPR,NST)
      GO TO 5
 160  CONTINUE
C     
C     GENERATE FILM PLOTS
C     
      IF(LATLON) THEN
         LLFLAG = 0
      END IF
      CALL PLTDRV(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,4),IBUF(1,7),
     X            MXFLSH,IPR,XSTA,YSTA,ZSTA,NNET,SMRK,NMRK,NET,
     X            IMRK,LATLON)
      GO TO 5
 170  CONTINUE
C     
C     READ IN A VOLUME FOR EDITING FROM THE INPUT TAPE
C     
      print *,'CEDRIC: ibuf(1,1-3),rbuf(1),map(1,3)=',
     +     ibuf(1,1),ibuf(1,2),ibuf(1,3)
      print *,'CEDRIC: lin,lpr,icord,nfmax=',
     +     lin,lpr,icord,nfmax
      print *,'CEDRIC: gfield=',gfield
      CALL READVL(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,3),
     X            LIN,LPR,ICORD,GFIELD,LATLON)
      print *,'CEDRIC: ibuf(1,1-3),rbuf(1),map(1,3)=',
     +     ibuf(1,1),ibuf(1,2),ibuf(1,3)
      print *,'CEDRIC: lin,lpr,icord,nfmax=',
     +     lin,lpr,icord,nfmax
      print *,'CEDRIC: gfield=',gfield
      GO TO 5
 180  CONTINUE
C     
C     RENAME EDIT FIELDS
C     
      CALL TRANSF(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,3),
     X            IBUF(1,5),IPR,NST,ICORD)
      GO TO 5
 190  CONTINUE
C     
C     REMAP THE COORDINATE SYSTEM
C     
      CALL REMAP(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,12),IPR,NST,
     X           IFLAT,IBUF(1,8),IBUF(1,9),IBUF(1,10),IBUF(1,11),
     X           LATLON)
      GO TO 5
 200  CONTINUE
C     
C     DERIVATION OF RADIAL VELOCITY ESTIMATES FROM COMPONENT FIELDS
C     
      CALL SAMPLR(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,3),IPR,NST)
      GO TO 5
 210  CONTINUE
C     
C     PERFORM ADVECTION OF HORIZONTAL PLANES
C     
      CALL SHFTER(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,3),IBUF(1,4),
     X            IPR,NST)
      GO TO 5
 220  CONTINUE
C     
C     GENERATE STATISTICS
C     
      CALL STATS(KRD,IBUF(1,1),IBUF(1,2),IPR)
      GO TO 5
 230  CONTINUE
C     
C     MULTIPLE DOPPLER RADAR SYNTHESIS
C     
      CALL SYNDRV(KRD,IBUF,MXBSYN,IPR,NST,ICORD,GFIELD)
      GO TO 5
 240  CONTINUE
C     
C     GENERATE THREE DIMENSIONAL DISPLAYS
C     
      CALL DSP3D(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,3),IBUF(1,6),IPR)
      GO TO 5
 250  CONTINUE
C     
C     MANIPULATION OF EDIT FILE FIELDS
C     
      CALL TRANSF(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,3),
     X            IBUF(1,5),IPR,NST,ICORD)
      GO TO 5
 260  CONTINUE
C     
C     UNFOLD RADIAL VELOCITY ESTIMATES IN TWO DIMENSIONS
C     
      CALL UNFOLD(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,3),IPR,NST)
      GO TO 5
 270  CONTINUE
C     
C     SET ALTERNATE WINDOW FOR DISPLAYS
C     
C     BUT FIRST COPY KRD TO AN INT ARRAY FOR WINDIJ ROUTINE
C     
      CALL WINDIJ(KRD,0)
      GO TO 5
 280  CONTINUE
C     
C     WRITE THE CURRENT VOLUME TO AN OUTPUT TAPE
C     
      IF(LATLON_KARD .EQ. 1) THEN
            ID(33) = ORIGINAL_LAT(1)
            ID(34) = ORIGINAL_LAT(2)
            ID(35) = ORIGINAL_LAT(3)
            ID(36) = ORIGINAL_LON(1)
            ID(37) = ORIGINAL_LON(2)
            ID(38) = ORIGINAL_LON(3)
      ENDIF

      print *,'CEDRIC - before WRITVL'
      CALL WRITVL(KRD,IBUF(1,1),MAXPLN,IBUF(1,3))
      print *,'CEDRIC - after WRITVL'
      GO TO 5
 290  CONTINUE
C     
C     CALCULATE CROSS-CORRELATION
C     
      CALL CROSS(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,3),IPR,NST)
      GO TO 5
 300  CONTINUE
C     
C     GENERATE 2-D PERSPECTIVE DISPLAYS
C     
      CALL DSPSRF(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,3),IPR)
      GO TO 5
 310  CONTINUE
C     
C     LINEAR REGRESSION OF ONE FIELD VERSUS ANOTHER
C     
      CALL REGRES(KRD,IBUF(1,1),IBUF(1,2),IPR)
      GO TO 5
 320  CONTINUE
C     
C     LAPLACIAN SOLUTION OF 2-D DIFFERENTIAL (PRESSURE RETRIEVAL BY LEVEL)
C     
      CALL LAPDRV(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,3),IPR,NST)
      GO TO 5
 330  CONTINUE
C     
C     WINDOWED TRANSFER
C     
      CALL TRANSF(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,3),
     X     IBUF(1,5),IPR,NST,ICORD)
      GO TO 5
 340  CONTINUE
C     
C     STATISTICAL PROFILES
C     
      CALL STPDRV(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,3),IPR)
      GO TO 5
 350  CONTINUE
C     
C     RELAXATION OF (U,V) COMPONENTS USING ADJUSTED W
C     
      CALL RLXDRV(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,3),IPR,NST)
      GO TO 5
 360  CONTINUE
C     
C     2-RADAR SOLUTION OF MASS-CONTINUITY EQUATION
C     
      CALL MS2DRV(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,3),IPR,NST)
      GO TO 5
 370  CONTINUE
C     
C     SELECT COORDINATE SYSTEM FOR PROCESSING
C     
      CALL COORD(KRD,ICORD)
      GO TO 5
 380  CONTINUE
C     
C     ENABLE/DISABLE FLAT EARTH MODE
C     
      CALL FLAT(KRD,IFLAT)
      GOTO 5
 390  CONTINUE
C
C     READ IN ASCII AIRCRAFT TRACK DATA
C
      CALL READAIR(KRD,XACT,YACT,ZACT,BEGACT,DELACT,NACT,DTAC,AIRTHK,
     X     UACT,VACT,WACT,IACTWND,IBUF(1,1),IBUF(1,3),LATLON)
      GO TO 5
 400  CONTINUE
C
C     READ IN ASCII STATION (MESONET) LOCATIONS
C
      CALL READSTA(KRD,XSTA,YSTA,ZSTA,IMRK,NET,NNET,SMRK,NMRK,LATLON)
      GOTO 5
 410  CONTINUE
C
C     PROCESS A COMMAND RELATED TO THE GUI
C
      CALL GUIPROC(KRD)
      GOTO 5
 420  CONTINUE
C
C     SET BACKGROUND/FOREGROUND COLORS
C
      CALL BCKGRND(KRD,LPR)
      GOTO 5
 430  CONTINUE
C
C     SET LAT/LON HEMISPHERES and ORIGIN
C
      print *,'CALLING LAT_LON'
      LATLON_KARD = 1
      CALL LAT_LON(KRD,LATSPHERE,LONSPHERE,LAT_SIGN,LON_SIGN,
     X     ORGLAT,ORGLON,ANGXAX)
      ORIGINAL_LAT(1) = ID(33)
      ORIGINAL_LAT(2) = ID(34)
      ORIGINAL_LAT(3) = ID(35)
      ORIGINAL_LON(1) = ID(36)
      ORIGINAL_LON(2) = ID(37)
      ORIGINAL_LON(3) = ID(38)
      IF(ORGLAT.NE.0.0)THEN
         ID(33) = INT(ORGLAT)
         ID(34) = INT((ORGLAT-ID(33))*60.0)
         ID(35) = NINT(((ORGLAT-ID(33))*60.0 - ID(34))*60.0*ID(68))
      END IF
      IF(ORGLON.NE.0.0)THEN
         ID(36) = INT(ORGLON)
         ID(37) = INT((ORGLON-ID(36))*60.0)
         ID(38) = NINT(((ORGLON-ID(36))*60.0 - ID(37))*60.0*ID(68))
      END IF
      print *,'Orig: ',original_lat(1),original_lat(2),original_lat(3)
      print *,'      ',original_lon(1),original_lon(2),original_lon(3)
      print *,orglat,orglon
      print *,' New: ',id(33),id(34),id(35)
      print *,' New: ',id(36),id(37),id(38)
      GOTO 5
 440  CONTINUE
C     
C     VAD ANALYSIS PROCEDURES
C     
      CALL VADDRV(KRD,IBUF(1,1),IBUF(1,2),IBUF(1,3),IBUF(1,15),MAXBSZ,
     X     IPR,NST,KVD,NAMINF,NAMOUF,NAMDBZ)
      GOTO 5
 450  CONTINUE
C
C     READ VAD PLOT SPECIFICATIONS AND DO THE PLOTS
C
      print *,'Before SAVVAD: nvdmx,mxvd=',nvdmx,mxvd
      CALL SAVVAD(KRD,VADTYPE,NAMPLVD,NAMFLD,JVD,NVD,ZMNVD,ZMXVD,
     X     ISKPVD,AZMVD,WFILT,XMNVD,XMXVD,XSCLVD,XREFVD,TYPVD,NVDMX,
     X     MXVD)
      print *,'After SAVVAD - back in CEDRIC, nvd,jvd=',nvd,jvd
      DO N=1,NVD
         WRITE(LPR,1700)N,NAMFLD(N),XMNVD(N,JVD),XMXVD(N,JVD),
     +        XREFVD(N,JVD),XSCLVD(N,JVD),TYPVD(N,JVD)
 1700    FORMAT(8X,'#',I2,' X AXIS: NAM,FMN-MX,REF,SCL= ',
     +        A8,4F8.2,4X,A4)
      END DO
      print *,'Before PLTVAD: jvd,nvd,kvd=',jvd,nvd,kvd
      ZRAD=0.001*ID(317)
      LABFLG=1
      CALL PLTVAD(NAMFLD,JVD,NVD,ZMNVD,ZMXVD,ISKPVD,WFILT,
     X     XMNVD,XMXVD,XSCLVD,XREFVD,TYPVD,MXVD,ZRAD,KVD,
     X     NAMINF,NAMOUF,NAMDBZ,LABFLG)
      print *,'After PLTVAD'
      GOTO 5
 460  CONTINUE
C
C     Read and plot MGLASS sounding
C
      CALL GETSND(KRD,OLAT,OLON,ANGXAX,ORGLAT,ORGLON,BAD)
      GOTO 5
 470  CONTINUE
C
C     Get NLD CG lightning (LAT,LON) positions for later overlaying
C
      CALL GETNLD(KRD,TNLD,XNLD,YNLD,PNLD,MXNL,INLD,OLAT,OLON,
     X     ANGXAX,ORGLAT,ORGLON,DTNLD,DZNLD,THKNLD)
      GOTO 5
 480  CONTINUE
C
C     Get LMA lightning channel (LAT,LON) positions for later overlaying
C
      CALL GETLMA(KRD,TLMA,XLMA,YLMA,ZLMA,MXLM,ILMA,OLAT,OLON,
     X     ANGXAX,DTLMA,ORGLAT,ORGLON,DZLMA)
      GOTO 5
 490  CONTINUE
C
C     Get WRF variables
C
      CALL GETVAR(KRD)
      GOTO 5
 500  CONTINUE
C
C     Interpolate WRF variables to constant pressure surfaces
C
c-----CALL PINTRPVAR(KRD)
      GOTO 5
 510  CONTINUE
C
C     Interpolate WRF variables to constant height surfaces
C
c-----CALL HINTRPVAR(KRD)
      GOTO 5

 520  CONTINUE
C
C     Extract soundings from current model dataset and plot
C
      CALL DATASND(KRD,LATLON,IBUF(1,1),IBUF(1,2),IBUF(1,3),IBUF(1,15),
     X            MAXBSZ,IPR)
      GOTO 5

 530  CONTINUE
C
C     Plot color table frames with the PLTCOLOR command (no parameters
C     are required (LJM 7/30/2011).
C
      CALL TSTCOL
      GOTO 5

 540  CONTINUE
C
C     Set line thickness using SETLINE command followed by
C     Thickness (F8.0) as a parameter (LJM 7/30/2011)
C     For example, SETLINE 1250.0 will set line thickness 
C     to 1250.  The line thickness has been reset to 1200
C     from the default value of 1000 which is too faint.
C
      READ (KRD(2),543)RJLW
 543  FORMAT(F8.0)
      JLW=INT(RJLW)
      CALL GETUSV('LW',ILW)
      CALL SETUSV('LW',JLW)
      print *,'Current line thickness ',ilw,' reset to ',jlw

      GOTO 5

 900  CONTINUE
C     
C     TERMINATE THE SESSION
C     
      CALL CLOCK(TYME)
      READ (TYME,11)IHR,IMN,ISC
      ENDSEC = ISC + 60*IMN + 3600*IHR
      TOTSEC = ENDSEC - BEGSEC
      TOTMIN = INT(FLOAT(TOTSEC)/60.0)
      REMSEC= TOTSEC - 60*TOTMIN
      PRINT *,' '
      WRITE(LPR,903)
 903  FORMAT(72('+'))
      WRITE(LPR,905)TYME,TOTSEC,TOTMIN,REMSEC
 905  FORMAT('CEDRIC EXECUTION ENDED AT ',A8, 
     X     ' TAKING ',I6,' SECONDS OR MM:SS=',I3.2,':',I2.2)
      WRITE(LPR,903)

      CALL CEDERX(500,1)
      END
