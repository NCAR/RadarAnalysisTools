      PROGRAM PPI_MMM
c
c----------------------------------------------------------------------X
c
C     Biggest controls on memory needed: (MXR,MXA,MXF) and (MSMX,MDMX)
C
C     Set (MXR,MXA,MXF) in either dim_ppi.inc or dim_rhi.inc, depending
C        on (more ranges and angles, fewer functions) or vice versa
C        PARAMETER (MXR=768,MXA=450,MXF=20)
C
C     Set (NSMX,NDMX) in scat.inc
C        PARAMETER (NSMX=50,NDMX=300000,MXDS=NSMX*NDMX) - All ?
C
C  Initialize large storage arrays DAT, TMP1, and TMP2 to BDVAL=-999.
C     LINUX - Initialize big arrays during execution in do loops.  
C             GNU Compiler takes too long and virtual memory may 
C             not be big enough.
C     OTHERS- Initialize big arrays in data statements.
c----------------------------------------------------------------------X
C
C     NOTICE
C     Copyright 1993 University Corporation for Atmospheric Research
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
C publicity to endorse or promote any products or commercial entity unless 
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

C     DATA RE,REI/17000.0,1.17647E-04/
C     Height above a curved earth: Z=H0+SRNG*SINE+0.5*HRNG*HRNG*REI
C        where   H0   - height of the antenna feed
C              SRNG   - slant range
C              SINE   - sin(elevation angle)
C              HRNG   - horizontal range = SRNG*COSE
C              Rprime - 4/3 earth radius = 4/3 (6375) = 8500
C                       where Earth's radius is 6375 km from 
C                       Doviak and Zrnic, p. 15 Fig. 2.7
C              REI    - 1/Rprime = 1.17647E-04
C              RE     - 2*Rprime = 2*8500 = 17000
C
C     Maximum numbers allowed
C
C     MXR    - Range gates (200-1000, see dim.inc)
C     MXA    - Angles (100-750, see dim.inc)
C     MXF    - Fields, including input and new (10-60, see dim.inc)
C     NLIST  - Basic commands (55)
C     MXUF   - Input or output universal or dorade format fields (50)
C     MXUN   - Output universal format units (30)
C     NHMX   - Histograms (50)
C     NSMX   - Scatter plot specifications (100; 50 on others)
C     NDMX   - Accumulated scatter points per plot (300000; 50000 on others)
C     NPMX   - Spectra (25)
C     MXL    - Aircraft track input values (15000)
C     MXK    - Landmarks (12000)
C     MXNL   - NLD CG lightning strikes (100000)
C     MXLM   - LMA lightning channel (1000000)
C     MXSND  - MGLASS sounding levels (500)
C     MXT    - Maximum number of hours in inventory.txt file (72)
C     NAMX   - Pressure levels for adiabatic LWC (500)
C     NCMX   - Number of diabatic profiles (10)
C     NBMAX  - Angular (A,E) positions in a volume scan (6000)
C     NFXMAX - Fixed angles in a volume scan (100)
C     NVDMX  - VAD-analyzed fields (6)
C     MXVD   - VAD analyses (10 - see vadwinds.inc)
C     MXG    - Vertical levels for VAD analysis (200 - see vadwinds.inc)
C     MXPLT  - Contour and swath plots (400)
C
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      INCLUDE 'swth.inc'
      INCLUDE 'areatime.inc'
      INCLUDE 'colors.inc'
      INCLUDE 'vadwinds.inc'
      INCLUDE 'scat.inc'
      INCLUDE 'snding.inc'
      INCLUDE 'WORD.size'

      PARAMETER (MXRAF=MXR*MXA*MXF,MXRA=MXR*MXA,MXNF=3*MXF)

      PARAMETER (NLIST=55,MXUF=50,NHMX=50)
      PARAMETER (NPMX=25,MXL=15000,MXK=12000,MXT=72)
      PARAMETER (NAMX=500,NCMX=10,NBMAX=6000,NFXMAX=100)
      PARAMETER (NVDMX=6)
      PARAMETER (MXPLT=400)

      CHARACTER*8 LIST(NLIST),INDAT(10),SINDAT(10,MXPLT),SFUN(10,MXNF)
      CHARACTER*8 SWTHDAT(10,MXPLT)
      CHARACTER*3 WHICH_AHST,WHICH_ASCT
      DATA WHICH_AHST,WHICH_ASCT/'NON','NON'/
      CHARACTER*8 JNDAT(10)
      CHARACTER*8 IFMT
      CHARACTER*4 IPFLD,IDIR,PTYP
      CHARACTER*3 ISCTP(8),LABLS,SCANTYP
      CHARACTER*4 FRQAX(NPMX),AMPAX(NPMX),DTREND(NPMX)
      CHARACTER*4 ACFILT

      CHARACTER*6 SMRK(20)
      CHARACTER*7 NMRK(MXK),PLT_NMRK(MXK)
      CHARACTER*1 PLT_TIME(MXK,MXT),REC_DATA(MXK)
      CHARACTER*11 NEX_DATE
      CHARACTER*3 PERCENT(MXK)
      INTEGER HOUR_BEG,HOUR_END
     
C     Information in COMMON/NEXCLOCK/ passed to LABEL from 
C        PLT_CLOCK and PLT_PERCENT:
C
C        PLTCLOCK - NEXRAD data clock is plotted if .TRUE.
C        TIME_BEG - Beginning hour of NEXRAD data
C        TIME_END - Ending    hour of NEXRAD data
C        REQ_RADS - Number of NEXRADs requested
C        REC_RADS - Number of NEXRADs received
C        REC_HOURS- Total number of hours received
C        REQ_HOURS- Total number of hours requested
C        TPERCENT - Total percent requested/received
C        DATE_BEG - Beginning date of NEXRAD data
C        DATE_END - Ending    date of NEXRAD data
C        NEX_NAME - NEXRAD radar name for plot label
C        PLTPERCNT- NEXRAD data percent is plotted if .TRUE.
C
      PARAMETER (MXKK=1000)
      LOGICAL PLTCLOCK,PLTPERCNT
      INTEGER TIME_BEG,TIME_END,REQ_RADS,REC_RADS,REC_HOURS,REQ_HOURS
      CHARACTER*11 DATE_BEG,DATE_END
      CHARACTER*14 NEX_NAME(MXKK)
      COMMON/NEXCLOCK/PLTCLOCK,PLTPERCNT,TIME_BEG,TIME_END,
     X     REQ_RADS,REC_RADS,REQ_HOURS,REC_HOURS,TPERCENT,
     X     DATE_BEG,DATE_END,NEX_NAME

      DATA PLTCLOCK,PLTPERCNT/.FALSE.,.FALSE./

      CHARACTER*1 BGFLAG,ITRANS
      CHARACTER*8 BGNAMES(7)
      CHARACTER*8 ICOLTYP,IGRYCON
      CHARACTER*8 TS_LL,TS_SIZ
      CHARACTER*8 GRDTYP
      CHARACTER*4 TYPVD(NVDMX,MXVD)
      CHARACTER*60 LABSTDIN(12)
      CHARACTER*4 GRAYTYP
      CHARACTER*6 PLTABL
      CHARACTER*2 DIGCOLR,SCTCOLR,HSTCOLR
      CHARACTER*8 PLTLAST
      CHARACTER*8 HMATCH
      CHARACTER*4 SMATCH
      LOGICAL FOF,UNI,COLRFIL,FRSTREC
      LOGICAL PLTSW,PROCESS,VECTS,WINSET
      LOGICAL AHST,AHSTCLR,ASCT,ASCTCLR,PLTEOV,SETGRD
      REAL NSMIN,NSMAX

      COMMON/ANGLS/ASCAN(MXA),ANGINC(MXA),FXELC(MXA),FXERR(MXA),AVGI
      COMMON /FILCH/FLSPAC(MXF)
      CHARACTER*8 FLSPAC
      COMMON/FIL/IFILTER(MXF),IGATE(MXF),DXX(MXF),DYY(MXF)
      COMMON /INPUTCH/NAMFLD(MXF),IRATYP,ICORD
      CHARACTER*8 NAMFLD,IRATYP,ICORD
      COMMON/BCN/RNGMIN,RNGMAX,TYMN,TYMX,IBSCAN,TYMSCL
      COMMON /ORIGINCH/NETWORK
      CHARACTER*8 NETWORK
      COMMON/ORIGIN/X0,Y0,H0,AZCOR,BAZ,XRD,YRD
      COMMON /PLTWIN/XRT(26),YTP(26),SIDEX(26),SIDEY(26),DXL,DXR,DYB,
     + DYT,NROW,NCOL,NWIN,IWIN,LABX(26),LABY(26),ILFLG,XBTP(26),
     + YBTP(26),SIZBX,XSHIFT,YSHIFT
      COMMON /INPUTC/ IPFLD
      COMMON/LIMITS/AZB,AZE,AZV
      COMMON/SCRATCH/TMP1(MXR),TMP2(MXR,MXA)
      COMMON/LWC/PBASE(NCMX),TBASE(NCMX),ZBASE(NCMX),B1(NCMX),
     X     B2(NCMX),B3(NCMX),HMAX(NCMX),NAL(NCMX),PALT(NAMX,NCMX),
     X     ADBLWC(NAMX,NCMX),ADBDBZ(NAMX,NCMX),ADBDIA(NAMX,NCMX),
     X     ADBCON(NAMX,NCMX),NC

      COMMON/BADT/NFILE
C
C     COMMON block variables returned from LAT_LON or uses defaults.
C
      COMMON /HEMISPHERE/LATSPHERE,LONSPHERE,LAT_SIGN,LON_SIGN
      CHARACTER*8 LATSPHERE,LONSPHERE
      REAL LAT_SIGN,LON_SIGN
      REAL MLAT,MLON
      DATA LATSPHERE,LONSPHERE/'NORTH','WEST'/
      DATA LAT_SIGN,LON_SIGN/+1.0,+1.0/

      INTEGER USER_DEF_ORIGIN,RADAR_CHOSEN
      CHARACTER*8 EXP,RADAR_NAME,ORIGIN_NAME

      CHARACTER*8 NAMNEX(MXF_NEX)
      CHARACTER*8 NAMUF(MXUF),NAMOUT(MXUF),NMOUTUF(MXUF)
      CHARACTER*8 IRNAM(MXF),IANAM(MXF),IHNAM(NHMX)
      CHARACTER*4 HTYP(NHMX),HFIT(NHMX)
      CHARACTER*8 IXNAM(NSMX),IYNAM(NSMX),IPNAM(NPMX),APROC(MXF)
      DIMENSION IUFUN(20)
      DIMENSION RFMN(MXF),RFMX(MXF),RREF(MXF,5),RPROC(MXF)
      DIMENSION AFMN(MXF),AFMX(MXF),AREF(MXF),ACNT(MXF),AGAP(MXF),
     +    AERR(MXF)
      CHARACTER*8 APRNT(MXF),RPRNT(MXF),LSTNAM(MXF),NAMLST(MXF)
      CHARACTER*8 LTYP
      DIMENSION FMN(NHMX),FMX(NHMX),FBIN(NHMX),FREF(NHMX,2)
      DIMENSION PMN(NHMX),PMX(NHMX)
      DIMENSION XFMN(NSMX),XFMX(NSMX)
      DIMENSION YFMN(NSMX),YFMX(NSMX)
      DIMENSION SLOP(NSMX),YCEPT(NSMX)
      DIMENSION RLAG(NSMX),ALAG(NSMX)
      INTEGER*2 XDAT(NDMX,NSMX),YDAT(NDMX,NSMX)
      DIMENSION NDAT(NSMX)
      DIMENSION XMNVD(NVDMX,MXVD),XMXVD(NVDMX,MXVD)
      DIMENSION XSCLVD(NVDMX,MXVD),XREFVD(NVDMX,MXVD),WFILT(MXVD)
      DIMENSION ZMNVD(MXVD),ZMXVD(MXVD),ISKPVD(MXVD)
      DIMENSION FRQMN(NPMX),FRQMX(NPMX)
      DIMENSION PEXMN(NPMX),PEXMX(NPMX),PTAVG(NPMX)
      DIMENSION FLDMN(NPMX),FLDMX(NPMX),FLDRF(NPMX)
      DIMENSION XACT(MXL),YACT(MXL),ZACT(MXL),TACT(MXL)
      DIMENSION UACT(MXL),VACT(MXL),WACT(MXL)
      DIMENSION CACT(MXL),QACT(MXL),DACT(MXL),HACT(MXL)
      DIMENSION XMRK(MXK),YMRK(MXK),ZMRK(MXK),AMRK(MXK),NNET(20)

C     Common blocks for NLDN and LMA positional information
C
      PARAMETER (MXNL=100000,MXLM=1000000)
      COMMON/NLDN/XNLD(MXNL),YNLD(MXNL),TNLD(MXNL),PNLD(MXNL),
     X            HNLD(MXNL),AZNLD(MXNL),INLD,DTNLD
      COMMON/LMA/XLMA(MXLM),YLMA(MXLM),ZLMA(MXLM),TLMA(MXLM),
     X           HLMA(MXLM),AZLMA(MXLM),ILMA,DTLMA
      CHARACTER*4 LMA_COLR

      DIMENSION AZVOL(NBMAX),ELVOL(NBMAX),FXVOL(NFXMAX)
      DIMENSION JHSK(256),IHSK(256)
      INTEGER RADAR_TYPE
      CHARACTER*8 AVNAM,FLTNUM
      CHARACTER*3 ROTATE
      DATA ROTATE/'YES'/
      DATA AVNAM/'??????? '/
      DATA INDAT/10*'??????? '/

c----------------------------------------------------------------------X
C  Initialize large storage arrays DAT, TMP1, and TMP2 to BDVAL=-999.
c----------------------------------------------------------------------X
C     LINUX (GNU) - Initialize big arrays internally during execution.  
C             Compiler takes too long and virtual memory may not be big 
C             enough.  Comment out these DATA initializations if GNU
C
c      DATA DAT  /MXRAF*-999./
c      DATA TMP1 /MXR*-999./
c      DATA TMP2 /MXRA*-999./
c----------------------------------------------------------------------X
      DATA BDVAL/-999./
      DATA NAMFLD /MXF*'??????? '/
      DATA NDAT/NSMX*0/

      DATA LIST/'INPUT   ','CONTOUR ','PPIWIN  ','RHIWIN  ','SURWIN  ',
     +          'COPWIN  ','DISPOSE ','PLTSWTH ','FUN     ','BSCAN   ',
     +          'PLTANGL ','PLTRNGE ','TITLE   ','PLTHIST ','PLTSCAT ',
     +          'GETCAL  ','DUMP    ','PLTSCAN ','VECTOR  ','GRADF   ',
     +          'LABELS  ','SETWIN  ','RESET   ','PLTVAD  ','PLTSPEC ',
     +          'GETACT  ','GETMRK  ','UFOUT   ','BCKGRND ','SETLWC  ',
     +          'CONTHIK ','GRID    ','CNTSWTH ','WINSWTH ','BCKSWTH ',
     +          'PLTAE   ','RNGCOR  ','MACHSIZ ','PLTPROJ ','PLTAHST ',
     +          'PLTASCT ','LISTFLD ','SETGRAY ','SETGRID ','LATLON  ',
     +          'ORIGIN  ','PLTNET  ','NEXCLOK ','NEXPCNT ','GETNLD  ',
     +          'GETLMA  ','GETSND  ','PLTHZ   ',
     +          'PROCESS ','STOP    '/
      DATA ISCTP/'PPI','COP','RHI','VER','TAR','MAN','IDL','SUR'/
      DATA SCANTYP/'???'/
      DATA NGTSOLD/0/
      DATA DROLD/-99./
      DATA NF,NFLDS,MSCAN,MNANG,NP,ISWATH/0,0,0,5,0,0/
      DATA XMIN,YMIN,XMAX,YMAX/16*0.,16*40./
      DATA IARCS,ITPFLG,FXMN,FXMX/16*0,16*-1./
      DATA ANGTOL/8*0.5/
      DATA RNG1,RNG2,HGT1,HGT2/0.,40.,0.,15./
      DATA ITERGT,ITERBM,ISHADE,IEOS,IEOT,NOSCAN/1,1,0,0,0,0/
      DATA ELVMN,ELVMX,AZMN,AZMX/0.,90.,0.,360./
      DATA COLRFIL,PLTSW,WINSET/.FALSE.,.FALSE.,.FALSE./
      DATA IUNOLD,FRSTREC,PROCESS/-1,.TRUE.,.FALSE./
      DATA P,Q/1,2/
      DATA IPREC,NANG(1),NANG(2)/0,1,MXA/
      DATA IFD,NDUMP,NRST,LABLS/0,0,20,'ALL'/
      DATA IGRPLT,IOLDGY/0,0/
      DATA IBLACK,IWHITE,IGRAY,IRED,IGREEN,IBLUE/63,64,65,66,67,68/
      DATA ICYAN,IMAGENTA,IYELLOW/69,70,71/
      DATA IBMAGENT,ISBLUE,IORANGE,IFGREEN,INBLUE/72,73,74,75,76/
c      DATA XRT(1),YTP(1),SIDEX(1),SIDEY(1)/0.92,0.94,0.84,0.84/
      DATA XRT(1),YTP(1),SIDEX(1),SIDEY(1)/0.90,0.94,0.80,0.80/
      DATA NROW,NCOL,NWIN,IWIN/1,1,1,1/
      DATA LABX(1),LABY(1),ILFLG/1,1,0/
      DATA IAZC,FRSTBM,JBEAM/.FALSE.,.FALSE.,0/
      DATA NA/0/
      DATA JBSWT/MXA*0.0/
      DATA NSHORT,MXSHORT/0,10/
      DATA NEWDAY/0/
      DATA FXOLD/-99./
      DATA MVD,JVD/0,0/
      DATA RNGCOR/0.0/
      DATA NSWPAVG,PLTEOV,SETGRD/999999,.FALSE.,.FALSE./
      DATA AHST,AHSTCLR,ASCT,ASCTCLR/.FALSE.,.FALSE.,.FALSE.,.FALSE./
      DATA NSWTH,NBVOL,NFXVOL,NFXHST,NFXSCT/0,0,0,0,0/
      DATA GRAYTYP,GSTR,GRAYEST/'LIN ',0.95,0.0/
      DATA ANGXAX/90.0/
      DATA BGNAMES/'Black   ',
     +             'White   ',
     +             'Cyan    ',
     +             'Sblue   ',
     +             'Lcyan   ',
     +             'Gray    ',
     +             'Nblue   '/
      DATA BGFLAG/'W'/
      SAVE BGFLAG

C  Begin execution of program
C
      CALL VERDATE(LABSTDIN)

c----------------------------------------------------------------------X
C  Initialize big arrays during execution - GNU compiler takes too long 
C  LINUX (GNU) - Leave these DO loops in when compiling 
c----------------------------------------------------------------------X
C
      PRINT *,'LINUX (GNU) - Initializing big arrays DAT, TMP1,',
     +     ' and TMP2'
      DO K=1,MXF
         DO J=1,MXA
            DO I=1,MXR
               TMP1(I)=BDVAL
               TMP2(I,J)=BDVAL
               DAT(I,J,K)=BDVAL
            END DO
         END DO
      END DO
      PRINT *,'LINUX (GNU) - Finished initialization of big arrays'
c----------------------------------------------------------------------X
C  Initialize machine read on/written on and word size from WORD.size 
C     parameter list.
C     Using default values from WORD.size; also see routine MACHSIZ.
C
      DEC=IDEC
      DECWR=IDECWR
      WORDSZ=IWORDSZ

      WRITE(6,1011)MXR,MXA,MXF,MXNF,MXPLT,NHMX,NSMX
 1011 FORMAT(
     + 4X,'****************************************************',/,
     + 4X,'*  Dimensioned:                                    *',/,
     + 4X,'*              ',I6,' Range gates                  *',/,
     + 4X,'*              ',I6,' Angles                       *',/,
     + 4X,'*              ',I6,' Fields                       *',/,
     + 4X,'*              ',I6,' Functions                    *',/,
     + 4X,'*              ',I6,' Contour/Swath plots          *',/,
     + 4X,'*              ',I6,' Histograms                   *',/,
     + 4X,'*              ',I6,' Scatter plot lines           *',/,
     + 4X,'*--------------------------------------------------*',/,
     + 4X,'*  INITIALLY USING DEFAULTS FROM WORD.size         *',/,
     + 4X,'*     Add MACHSIZ command to change these          *',/,
     + 4X,'*                                                  *')

      WRITE(6,1013)IWORDSZ,IDEC,IDECWR
 1013 FORMAT(4X,'* WORD SIZE = ',I2,' bits and IDEC=',I1,
     +     ' IDECWR=',I1,10X,'*')
      IF(DEC.EQ.1.0)THEN
         WRITE(6,1015)
 1015 FORMAT(4X,'* Reading data on little Endian (DEC-like) machine *')
      ELSE
         WRITE(6,1017)
 1017 FORMAT(4X,'* Reading data on big Endian (SUN-like) machine    *')
      END IF
      
      IF(DECWR.EQ.1.0)THEN
         WRITE(6,1019)
 1019 FORMAT(4X,'* Data written on little Endian (DEC-like) machine *')
      ELSE
         WRITE(6,1021)
 1021 FORMAT(4X,'* Data written on big Endian (SUN-like) machine    *')
      END IF
      IF((DEC.EQ.1.0 .AND. DECWR.EQ.0.0).OR.
     +   (DEC.EQ.0.0 .AND. DECWR.EQ.1.0))THEN
         WRITE(6,1023)
 1023 FORMAT(4X,'*       **** BYTE-SWAPPING IS REQUIRED ****        *')
      END IF

      WRITE(6,1025)
 1025 FORMAT(
     + 4X,'****************************************************',/)
C  Open GKS (OUTPUT UNIT=2)
C  Open GFLASH (WS ID=55, LUNIT=7, MAPFIL=3, i.e. GNFB03)
C
      IDWKS=55
      IUFL=7
      MAPFIL=3
      CALL OPNGKS
      CALL GOPWK(IDWKS,IUFL,3)
      CALL GSCLIP(0)
      NFRAME=0
      NC=0

C  Change function code delimiter for ':' to '&'
C  so that we can print a colon on the plots.
C
      CALL PCSETC ('FC', '&')

C     Get default line thickness (ILW) and reset to JLW.
C     Restore line thickness before leaving routine.
C
      CALL GETUSV('LW',ILW)
      JLW=1200
      IF(JLW.LT.ILW)JLW=ILW
      CALL SETUSV('LW',JLW)
      print *,'Default line thickness ',ilw,' reset to ',jlw
      print *,' '


C     CREATE THE FRAMES WITH THE ENTIRE INPUT STREAM
C     Uses the default of white lettering on a black background
C
c     Possible background colors:
c        Black, White, Cyan, Sblue, Lblue, and Gray
      IF(BGFLAG.EQ.'B')THEN
         INDX_BG=1
      ELSEIF(BGFLAG.EQ.'W')THEN
         INDX_BG=2
      ELSEIF(BGFLAG.EQ.'C')THEN
         INDX_BG=3
      ELSEIF(BGFLAG.EQ.'S')THEN
         INDX_BG=4
      ELSEIF(BGFLAG.EQ.'L')THEN
         INDX_BG=5
      ELSEIF(BGFLAG.EQ.'G')THEN
         INDX_BG=6
      ELSEIF(BGFLAG.EQ.'N')THEN
         INDX_BG=7
      ENDIF
      write(6,770)BGNAMES(INDX_BG),bgflag
 770  format(' Default background set to ',a8,' BGFLAG = ',a1)
      CALL MYSTDIN(LABSTDIN,INDAT,NFRAME,BGFLAG)

C  SET THE COLOR TABLE TO DEFAULT RAINBOW, WITH BLACK BACKGROUND
C     IGRPLT - (0) NOT GRAYTONES, (1) GRAYTONES, BCKGRND=BLACK
C     INITIAL VALUES: IGRPLT=0, BGFLAG=' '
C
      IGRPLT=0
      print *,'PPI_MMM before SETCOL: CALL SETBCKGRND, bgflag=',bgflag
      CALL SETCOL(IGRPLT,BGFLAG,GRAYTYP,GSTR,GRAYEST)
      print *,'SETCOL: Bg,gr=',bgflag,igrplt

C     TOP OF THE INPUT LOOP
C
 5    CONTINUE
      READ(5, 7)(INDAT(I),I=1,10)
 7    FORMAT(10A8)
c-----debug (ljm)
c      write(6,8)(indat(i),i=1,10)
c 8    format('Kardin=',10A8)
c-----debug (ljm)
      IF(INDAT(1)(1:1).EQ.'*')GO TO 5
      IGO=IFIND(INDAT(1),LIST,NLIST)
c------print *,'  igo=',indat(1),igo
      IF(IGO.EQ.0)THEN
         WRITE(6,9)(INDAT(I),I=1,10)
 9       FORMAT(1X,'INVALID COMMAND: ',10A8)
         GO TO 900
      END IF
      IF(INDAT(1)(1:4).EQ.'STOP')THEN
         ISTOP=1
         WRITE(6,91)NFRAME
 91      FORMAT(4X,' STOP PROCESSING:    FRAMES PLOTTED=',I6)
c         print *,'Why is program continuing to process?'
c         print *,'after stop: ',pltlast,istop,nswpavg,nswth,
c     +        nbvol,nfxvol,nfxhst,nfxsct
         IF( (ISTOP.EQ.1) .OR.
     X       (PLTLAST.EQ.'ENDVOL') .OR.
     X       (NSWPAVG.EQ.1))THEN
            print *,'The program will not continue to process'
c            print *,'ISTOP,PLTLAST,NSWPAVG=',istop,pltlast,nswpavg
c            print *,' Going to 900'
            GO TO 900
         END IF
         NSWPAVG=1
         IF(NSWTH .GE.1 .OR. 
     X      NBVOL .GE.1 .OR. 
     X      NFXVOL.GE.1 .OR. 
     X      NFXHST.GE.1 .OR. 
     X      NFXSCT.GE.1)THEN
            print *,'Going to 824'
            GO TO 824
         ELSE
            print *,'Going to 900'
            GO TO 900
         END IF
      ELSE
         ISTOP=0
      END IF
      GO TO ( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     +       110,120,130,140,150,160,170,180,190,190,
     +       200, 20,290,210,220,230,240,300, 20,320,
     +        20,125, 24, 24, 24, 24, 12, 16, 24, 24,
     +        24,122,430,440,450,460,470,480,490,500,
     +       510,520,24,
     +       800,800,824) IGO

C     INPUT CONTROL PARAMETERS-DATA FORMAT, EXPERIMENT NAME, RADAR
C     NAME, COORDINATE SYSTEM, NO OF ARCS, AND NO OF INPUT FILES
C
   10 CALL INPUT1(INDAT,IFMT,NETWORK,IRATYP,ICORD,
     X     X0,Y0,XRD,YRD,H0,AZCOR,BAZ,ANGXAX,IFORBLK)
      FRSTREC=.TRUE.
      PROCESS=.FALSE.
      print *,'Finished with INPUT1'
      GO TO 5

C     INPUT RANGE CORRECTION TO FIRST GATE
C
   12 WRITE(6,13)(INDAT(I),I=1,10)
   13 FORMAT(1X,10A8)
      READ(INDAT,14)RNGCOR
   14 FORMAT(/F8.0)
      GO TO 5

C     INPUT MACHINE (READING ON/DATA WRITTEN ON) AND WORD SIZE PARAMETERS
C
   16 CALL MACHSIZ(INDAT,DEC,DECWR,WORDSZ)
      GO TO 5

C     INPUT FIELD TO CONTOUR, TYPE OF PLOT (BLACK-WHITE OR COLOR AREA FILL),
C     RANGE GATE SKIP FACTOR FOR PLOTTING, AND CONTOUR LEVELS
C
   20 CALL SAVPLT(INDAT,IFMT,SINDAT,NAMFLD,IFLD,NFLDS,NP,MXPLT)
      IF(INDAT(1).EQ.'BCKGRND ')THEN
         print *,'PPI_MMM after SAVPLT: CALL SETBCKGRND'
         BGFLAG=INDAT(2)(1:1)
         CALL SETBCKGRND(BGFLAG)
      ENDIF
      IF(INDAT(1).EQ.'CONTOUR ' .OR. INDAT(1).EQ.'CONTHIK ')THEN
         VECTS=.FALSE.
         CALL CONLEV(INDAT,NAMFLD,IFLD,IFL,ISW,COLRFIL,ICOLTYP,
     X               ITERGT,CL,NL,CMIN,CMAX,CINC,ISHADE,IOV,
     X               DRSW,DROLD,IBSCAN,PLTSW,IGRPLT,IGRYCON,
     X               JMAP,JACT,JMRK,JNLD,JLMA,NOLAB,THIK,MXF,SINDAT,
     X               MP,MXPLT,PROCESS,ITERBM,DIGMIN,DIGMAX,DIGOFF,
     X               DIGCOLR,DIGSIZE,ROTATE,BGFLAG)
      END IF
      GO TO 5

C     INPUT FIELD TO CNTSWTH, TYPE OF PLOT (BLACK-WHITE OR COLOR AREA FILL),
C     RANGE GATE SKIP FACTOR FOR PLOTTING, AND CONTOUR LEVELS
C
   24 CALL SAVSWTH(INDAT,SWTHDAT,NSWTH,NSWPAVG,MXPLT,WHICH_AHST,
     X     WHICH_ASCT)
      SCANTYP='PPI'
      GO TO 5

C     INPUT (X,Y) PLOT AND ELEVATION WINDOWS FOR PPI SCAN MODE
C
   30 CALL PPIWIN(INDAT,IRATYP,ICORD,XMIN,XMAX,YMIN,YMAX,GXMIN,GXMAX,
     X            GYMIN,GYMAX,ANGTOL,FXMN,FXMX,ICVRT,X0,Y0,AZCOR,IARCS,
     X            IAZC,AZMIN,AZMAX,AZROT,ITPFLG)
      IF(ITPFLG(1).EQ.1)THEN
         WINSET=.TRUE.
      ELSE
         WINSET=.FALSE.
      END IF
      print *,'winset=',winset
      GO TO 5

C     INPUT (R,Z) PLOT AND AZIMUTH WINDOWS FOR RHI SCAN MODE
C
   40 CALL RHIWIN(INDAT,IRATYP,ICORD,XMIN,XMAX,YMIN,YMAX,GXMIN,GXMAX,
     X            GYMIN,GYMAX,ANGTOL,FXMN,FXMX,ZSTR,ITPFLG)
      IF(ITPFLG(3).EQ.1)THEN
         WINSET=.TRUE.
      ELSE
         WINSET=.FALSE.
      END IF
      print *,'Finished with RHIWIN'
      GO TO 5

C     INPUT (X,Y) PLOT AND ELEVATION WINDOWS FOR SUR SCAN MODE
C
   50 CALL SURWIN(INDAT,IRATYP,ICORD,XMIN,XMAX,YMIN,YMAX,GXMIN,GXMAX,
     X            GYMIN,GYMAX,ANGTOL,FXMN,FXMX,ICVRT,X0,Y0,AZCOR,IARCS,
     X            IAZC,AZMIN,AZMAX,AZROT,ITPFLG)
      IF(ITPFLG(8).EQ.1)THEN
         WINSET=.TRUE.
      ELSE
         WINSET=.FALSE.
      END IF
      print *,'Finished with SURWIN'
      GO TO 5

C     INPUT (X,Y) PLOT AND ELEVATION WINDOWS FOR COPL SCAN MODE
C
   60 CALL COPWIN(INDAT,IRATYP,ICORD,XMIN,XMAX,YMIN,YMAX,GXMIN,GXMAX,
     X            GYMIN,GYMAX,ANGTOL,FXMN,FXMX,ICVRT,X0,Y0,AZCOR,IARCS,
     X            IAZC,AZMIN,AZMAX,AZROT,ITPFLG)
      IF(ITPFLG(2).EQ.1)THEN
         WINSET=.TRUE.
      ELSE
         WINSET=.FALSE.
      END IF
      print *,'Finished with COPWIN'
      GO TO 5

C     DISPOSE META CODE FILE
C
   70 IF(PROCESS)THEN
         CALL DISP(INDAT,NFRAME,BGFLAG,GRAYTYP,GSTR,GRAYEST)
         PROCESS=.FALSE.
         GO TO 5
      ELSE
         GO TO 970
      END IF

C     PLOT THE ACCUMULATED SWATHS, ISOCHRONS OR INTEGRATIONS
C
 80   IF(PROCESS)THEN
         call sflush
         CALL PLTSWAT(INDAT,ZSTR,PLTSW,VECTS,NFRAME,IGRPLT,IGRYCON,
     X        JMAP,JACT,JMRK,JNLD,BGFLAG,SINDAT,MP,MXPLT,DELAMX,
     X        PROCESS,NFXVOL,COLRFIL,NOLAB,ICOLTYP,ROTATE,X_QUANT,
     X        Y_QUANT)
         IF(INDAT(4).NE.'RESET   ')THEN
            IF(NWIN.LE.1)THEN
               CALL LABEL(ZSTR,COLRFIL,PLTSW,VECTS,NFRAME,LABLS,
     X              IGRPLT,BGFLAG,NOLAB,ICOLTYP)
            ELSE
               CALL LABELWIN(ZSTR,COLRFIL,PLTSW,VECTS,NFRAME,LABLS,
     X              IGRPLT,BGFLAG,ICOLTYP)
            END IF
            IF(IOV.NE.1.AND.NWIN.GT.1)THEN
               IF(IWIN .EQ. (NWIN+1))WRITE(6,81)NFRAME
 81            FORMAT('  PLTSWTH:   FRAMES PLOTTED =',I6)
               IWIN=MOD(IWIN,NWIN+1)+1
               IF(IWIN.EQ.1)IWIN=2
            END IF
            PLTLAST='PLTSWATH'
         END IF
         GO TO 5
      ELSE
         GO TO 980
      END IF

C     SAVE ALL FUNCTION SPECIFICATIONS IN SFUN TO BE EXECUTED LATER
C
   90 CALL SAVFUN(INDAT,IFMT,SFUN,NAMFLD,IFLD,NFLDS,MXNF,NF,DAT,BDVAL,
     X     AZA,ELA,NANG,MVD,NAMVD,NAMINVD,MXVD,MXR,MXA,MXF,SCANTYP)
c      print *,'swath-scantyp=',scantyp
      GO TO 5

C     READ RANGE SPECIFICATIONS FOR BSCAN PLOTS
C
  100 CALL BSCN(INDAT,RNGMIN,RNGMAX,TYMN,TYMX)
      WINSET=.TRUE.
      GO TO 5

C     SAVE ALL CONSTANT RANGE PLOT SPECIFICATIONS TO BE EXECUTED LATER
C
  110 CALL SAVANGL(INDAT,NAMFLD,IFLD,NFLDS,PAMNA,PAMXA,PRMNA,PRMXA,
     X             RSKIP,IANAM,AFMN,AFMX,AREF,APROC,ACNT,AGAP,AERR,
     X             APRNT,NAP,LTYP)
      GO TO 5

C     SAVE ALL CONSTANT ANGLE PLOT SPECIFICATIONS TO BE EXECUTED LATER
C
  120 CALL SAVRNGE(INDAT,NAMFLD,IFLD,NFLDS,PRMNR,PRMXR,PAMNR,PAMXR,
     X             ASKIP,IRNAM,RFMN,RFMX,RREF,RPRNT,NRP,BDVAL,LTYP)
      GO TO 5

C     SAVE ALL LIST FIELD SPECIFICATIONS TO BE EXECUTED LATER
C
 122  CALL SAVLIST(INDAT,NAMFLD,NFLDS,MXF,PRMNL,PRMXL,PAMNL,PAMXL,
     X     RSKIPL,ASKIPL,LSTNAM,NAMLST,NLST,PZMNL,PZMXL)
      GO TO 5

C     CREATE THE GRID FRAME
C
  125 CONTINUE
      CALL MYGRID(INDAT,NFRAME,BGFLAG)
      IGRPLT=0
      BGFLAG='B'
      CALL SETCOL(IGRPLT,BGFLAG,GRAYTYP,GSTR,GRAYEST)
      print *,'SETCOL: Bg,gr=',bgflag,igrplt
      GO TO 5

C     CREATE THE TITLE FRAME
C
  130 CONTINUE
      BGFLAG=INDAT(2)(1:1)
      CALL MYTITLE(LABSTDIN,INDAT,NFRAME,BGFLAG)
      GO TO 5

C     SAVE ALL HISTOGRAM PLOT SPECIFICATIONS TO BE EXECUTED LATER
C
  140 CALL SAVHIST(INDAT,NAMFLD,IFLD,NFLDS,NAH,IHNAM,HRMN,HRMX,
     X             HAMN,HAMX,HZMN,HZMX,IHKP,JHKP,FMN,FMX,FBIN,
     X             FREF,PMN,PMX,HTYP,HFIT,HMATCH,NHMX,HSTCOLR)
      GO TO 5

C     SAVE ALL SCATTERGRAM PLOT SPECIFICATIONS TO BE EXECUTED LATER
C
  150 CALL SAVSCAT(INDAT,NAMFLD,IFLD,NFLDS,SRMN,SRMX,SAMN,SAMX,
     X             SZMN,SZMX,ISKP,JSKP,NAS,IXNAM,XFMN,XFMX,IYNAM,
     X             YFMN,YFMX,SLOP,YCEPT,RLAG,ALAG,NSMX,MXF,MSKP,NSKP,
     X             SMATCH,SCTCOLR)
      GO TO 5

C     GET THE PROCESSOR COUNTS-TO-DBM CALIBRATIONS
C
  160 CALL GETCAL(INDAT,CTDBM,CTDBZH,CTDBMXH,CTDBMXV,CTDBXH,CTDBXV,
     +   XHTHR,XVTHR)
      GO TO 5

C     SET FLAG (IFD) TO DUMP (NDUMP) BEAMS OF FOF DATA AND PRINT
C     EVERY NRST-TH BEAMS HOUSEKEEPING
C
  170 CALL DUMP(INDAT,IFD,NDUMP,NRST)
      GO TO 5

C     SAVE PARAMETERS FOR PLOTTING ACTUAL (FIXED) ANGLE VS. SCANNING ANGLE
C
  180 CALL SAVSCAN(INDAT,NSCAN)
      GO TO 5

C     INPUT CHARACTERISTICS FOR VECTOR OR GRADIENT PLOTS
C
  190 CALL SAVVEC(INDAT,IFMT,SINDAT,NAMFLD,IFLD,NFLDS,NP,MXPLT)
      GO TO 5

C     SET FLAG (LABLS) FOR ABBREVIATED ('ABR') OR ('ALL') LABELS
C     Also include as part of the contour command in order to change
C     the amount of labeling for different contour plots within the run.
C
 200  CALL LABELS(INDAT,LABLS)
      CALL SAVPLT(INDAT,IFMT,SINDAT,NAMFLD,IFLD,NFLDS,NP,MXPLT)
      GO TO 5

C     SAVE ALL VAD SCATTERGRAM PLOT SPECIFICATIONS TO BE EXECUTED LATER
C
  210 CALL SAVVAD(INDAT,VADTYPE,NAMPLVD,JVD,ZMNVD,ZMXVD,ISKPVD,
     X     U_VD,V_VD,AZMVD,WFILT,NVD,XMNVD,XMXVD,XSCLVD,XREFVD,
     X     TYPVD,NVDMX,MXVD)
      GO TO 5

C     SAVE ALL SPECTROGRAM PLOT SPECIFICATIONS TO BE EXECUTED LATER
C
  220 CALL SAVSPEC(INDAT,NAMFLD,IFLD,NFLDS,NSP,IPNAM,PRMN,PRMX,
     X             PAMN,PAMX,IPKP,JPKP,PTYP,SPAVG,FRQMN,FRQMX,
     X             PEXMN,PEXMX,FRQAX,AMPAX,DTREND,PTAVG,FLDMN,
     X             FLDMX,FLDRF)
      GO TO 5

C     GET AIRCRAFT (LAT,LON) POSITIONS FOR LATER OVERLAYING
C
  230 CALL GETACT(INDAT,XACT,YACT,ZACT,UACT,VACT,WACT,CACT,QACT,
     X     DACT,HACT,TACT,DTAC,DPAC,IACT,OLAT,OLON,ANGXAX,TMJR,
     X     TMNR,TS_LL,TS_SIZ,VECSCL,SCLR,WTYM,WMN,WMX,MXL,ORLAT,
     X     ORLON)
      GO TO 5

C     GET LANDMARK (LAT,LON) POSITIONS AND NAMES FOR LATER OVERLAYING
C
  240 CALL GETMRK(INDAT,NMRK,XMRK,YMRK,ZMRK,AMRK,MXK,IMRK,NET,NNET,
     X     SMRK,OLAT,OLON,ANGXAX,CMRK,ORLAT,ORLON)
      KMRK=IMRK
      GO TO 5

C     RESET ALL COUNTERS: SAVPLT (NP), SAVFUN (NF), SAVANGL (NAP),
C                         SAVRNGE (NRP), SAVHIST (NAH), SAVSCAT (NAS),
C                         FIELDS (NFLDS), SAVSCAN (NSCAN), SAVVAD (NVD),
C                         SAVSPEC (NSP), SAVSWTH (NSWTH), RD's (NBVOL),
C                         PLTHIST (NFXHST), PLTSCAT (NFXSCT)
C                         
C     RESET ALL LOGICAL FLAGS
C
  290 CONTINUE
      IF(NA.GT.0)CALL PLOTAREA(IDATE)
      NA      = 0
      NP      = 0
      NF      = 0
      NAP     = 0
      NRP     = 0
      NAH     = 0
      NAS     = 0
      NLST    = 0
      NFLDS   = 0
      NSCAN   = 0
      MVD     = 0
      JVD     = 0
      NVD     = 0
      NSP     = 0
      NSWTH   = 0
      NBVOL   = 0
      NFXVOL  = 0
      NFXHST  = 0
      NFXSCT  = 0
      IGRPLT  = 0
      IOLDGY  = 0
      NSWPAVG = 999999
      PLTEOV  = .FALSE.
      AHST    = .FALSE.
      AHSTCLR = .FALSE.
      ASCT    = .FALSE.
      ASCTCLR = .FALSE.
      SCANTYP = '???'
c      BGFLAG  = ' '
      BGFLAG  = INDAT(2)(1:1)
      print *,'RESET OPS BCKGRND: bgflag=',bgflag
      CALL SETBCKGRND(BGFLAG)
      CALL SETCOL(IGRPLT,BGFLAG,GRAYTYP,GSTR,GRAYEST)
c-----print *,'SETCOL: Bg,gr=',bgflag,icrplt
      GO TO 5

C SET UP SPECS FOR UNIVERSAL FORMAT OUTPUT
C
 300  CALL UFSPEC(NAMOUT,NMOUTUF,IUFUN,NUMUF,NUMUN,IWRUF,NAMFLD,NFLDS,
     X     INDAT,GRDTYP,IWFOR)
      GO TO 5

C     SET COEFFICIENTS IN LWC = LWC(DBZ, Adiabatic LWC) RELATION
C
 320  CALL SETLWC(INDAT)
      GO TO 5

C     Set parameters for grayscale colors
C
 430  CALL SETGRAY(INDAT,GRAYTYP,GSTR,GRAYEST,PLTABL,NFRAME)
      GO TO 5

C     Generate regular range-angle grid for storage of swath'd fields
C        Angle: Runs amnsw to amxsw at dasw
C        Range: Runs  drsw to rmxsw at drsw
C
 440  CALL SETGRID(INDAT,RMNSW,RMXSW,DRSW,AMNSW,AMXSW,DASW,
     X     AZCLO_MX1,MXR,MXA,RNG,AZA,NRSW,NANG,SETGRD)
      GO TO 5

C     Set latitude (North, South) and longitude (East, West) hemispheres
C     in order to use sign conventions in LL2XY and XY2LL.  Also used
C     to set the latitude, longitude for the origin
C
 450  CALL LAT_LON(INDAT,LATSPHERE,LONSPHERE,LAT_SIGN,LON_SIGN,
     X     ORGLAT,ORGLON,ANGXAX)
      OLAT=ORGLAT
      OLON=ORGLON
      ORLAT=ORGLAT
      ORLON=ORGLON
      MLAT = LAT_SIGN*ABS(OLAT)
      MLON = -1.0*LON_SIGN*ABS(OLON)
      print *,'LATLON: Olat,Mlat=',olat,mlat
      print *,'LATLON: Olon,Mlon=',olon,mlon
      GO TO 5

C     Set Radar and Origin names, latitudes and longitudes.
C     When EXP='NEXRAD', accesses NEXRAD text file for NWS
C     4-character station designations.
C
 460  CALL USRORIGIN(INDAT,EXP,ORIGIN_NAME,ORLAT,ORLON,ORALT,
     X     RADAR_NAME,RADLAT,RADLON,RADALT,XRD,YRD,H0,ANGXAX,
     X     RADAR_CHOSEN)
      NETWORK=EXP
      IRATYP=RADAR_NAME
      ICORD=ORIGIN_NAME
      OLAT=ORLAT
      OLON=ORLON
      USER_DEF_ORIGIN = 1
      MLAT= LAT_SIGN*ABS(OLAT)
      MLON = -1.0*LON_SIGN*ABS(OLON)
      X0=XRD
      Y0=YRD
      print *,'ORIGIN: Olat,Mlat=',olat,mlat
      print *,'ORIGIN: Olon,Mlon=',olon,mlon
      print *,'ORIGIN:   Xrd,Yrd=',xrd,yrd
      print *,'ORIGIN:     X0,Y0=',x0,y0
      GO TO 5

C     Set specific site names within Network file to be plotted.
C
 470  CALL PLTNET(INDAT,PLT_NMRK,MXK,KMRK)
      GO TO 5

C     Get information for plotting NEXRAD data availability clocks
C
 480  CALL PLT_CLOCK(INDAT,PLT_NMRK,PLT_TIME,REC_DATA,MXK,MXT,KMRK,
     X     HCIRC,NEX_DATE,HOUR_BEG,HOUR_END,NHOURS)
      GO TO 5

C     Get information for plotting NEXRAD data availability clocks
C
 490  CALL PLT_PERCENT(INDAT,PLT_NMRK,MXK,KMRK,PERCENT,HCIRC)
      GO TO 5

C     GET NLD CG Lightning (LAT,LON) POSITIONS FOR LATER OVERLAYING
C
 500  CALL GETNLD(INDAT,TNLD,XNLD,YNLD,HNLD,AZNLD,PNLD,MXNL,INLD,
     X     OLAT,OLON,ANGXAX,CNLD,DTNLD,ORLAT,ORLON,DZNLD)
      GO TO 5

C     GET LMA Lightning Channel (LAT,LON) POSITIONS FOR LATER OVERLAYING
C
 510  CALL GETLMA(INDAT,TLMA,XLMA,YLMA,ZLMA,HLMA,AZLMA,MXLM,ILMA,
     X     LMA_COLR,OLAT,OLON,ANGXAX,DTLMA,ORLAT,ORLON,DZLMA)
      GO TO 5

C     GET SOUNDING FOR MAPPING TO RADAR SPACE
C
 520  CALL GETSND(INDAT,OLAT,OLON,ANGXAX,ORLAT,ORLON,NFRAME)
      GO TO 5
c
c----------------- BRANCH HERE TO START PROCESSING --------------------X
c
  800 CONTINUE

      print *,'winset=',winset
      IF(.NOT.(WINSET))GO TO 990
      PROCESS=.TRUE.
c      print *,'Before gettime'
      CALL GETTIME(INDAT,IUN,IBTIME,IETIME,IDTIME,ITRANS,IBSWEP,IESWEP,
     X             TANGMX,ANGINP,IRW,IFMT,DELAMX)

      IF(IUN.NE.IUNOLD)THEN
         FRSTREC=.TRUE.
         IF(IUNOLD.GT.0)CLOSE(IUNOLD)
         IUNOLD=IUN
         IPREC=0
         IEOT=0
         IF(IFMT.EQ.'UF      ')THEN
            CALL UFREC(IUN,IPREC,IEOF,IEOT,NAMUF,MXUF,NFLD,IFORBLK,
     X           DEC,DECWR,WORDSZ,NDUMP,IRATYP,ICORD)
            call sflush
            IF(IEOT.EQ.1)GO TO 5
            CALL FLDIDUF(NAMFLD,NFLDS,NAMUF,NFLD,IFLD)
         ELSE IF (IFMT.EQ.'DORADE') THEN
            CALL DOREC(IUN,IPREC,IEOF,IEOT,NAMUF,MXUF,NFLD,IRW,
     X           RADAR_TYPE,JSTAT)
            print *,'after calling DOREC'
            IF (IEOT.EQ.1)GO TO 5
            CALL FLDIDDO(NAMFLD,NFLDS,NAMUF,NFLD,IFLD)
         ELSE IF(IFMT.EQ.'FOF     ')THEN
            CALL FFREC(IUN,IPREC,IEOF,IEOT,IFORBLK,IYR,IMON,IDAY,IDATE,
     X           IFTIME,IVOLOLD,ISWPOLD,ITPOLD,FXOLD,R0,DROLD,NGTSOLD,
     X           IHSK,RNGCOR,DEC,DECWR,WORDSZ)
            IF(IEOT.EQ.1)GO TO 5
            CALL FLDIDFF(NAMFLD,NFLDS,IFLD)
            CALL PRVOLMFF(IHSK,IPREC,RNGCOR,NFRAME)
         ELSE IF(IFMT.EQ.'HRD     ')THEN
            CALL FLDIDHR(NAMFLD,NFLDS,IFLD)
         ELSE IF(IFMT.EQ.'LIDAR   ')THEN
            CALL FLDIDLID(NAMFLD,NFLDS,IFLD)
c         ELSE IF(IFMT.EQ.'NEXRAD  ')THEN
c            CALL FLDIDNEX(NAMFLD,NFLDS,NAMNEX,NFL_NEX,IFLD)
         END IF
      END IF

C     READ FIRST (NEXT) RADAR SCAN, COMPUTE FUNCTIONS AND DRAW ALL PLOTS
C     PROCESSING LOOP UNTIL END-OF-TAPE OR ENDING TIME
C
  810 IF(IFMT.EQ.'UF      ')THEN
         CALL RDUF(IUN,IPREC,FRSTREC,ZSTR,PLTSW,COLRFIL,
     X        VECTS,NFRAME,IFD,NDUMP,NRST,IBSWEP,IESWEP,TANGMX,
     X        ANGINP,IFORBLK,NEWDAY,IVOL,IVOLOLD,NAMUF,MXUF,
     X        ITIMBOV,ITIMEOV,AZVOL,ELVOL,NBMAX,NBVOL,NFXVOL,
     X        IEOV,IEOF,DEC,DECWR,WORDSZ)
         call sflush

      ELSE IF(IFMT.EQ.'HRD     ')THEN
         CALL RDHRD(IUN,DEC,DECWR,WORDSZ,IFD,NRST)

      ELSE IF(IFMT.EQ.'LIDAR   ')THEN
         CALL RDLIDAR(DEC,DECWR,WORDSZ)

      ELSE IF(IFMT.EQ.'NEXRAD  ')THEN
         CALL RDNEXRAD(IUN,IRW,DEC,DECWR,WORDSZ,IFD,NRST,
     X        NDUMP,IBSWEP,IESWEP,ZSTR,PLTSW,COLRFIL,VECTS,
     X        NFRAME,LABLS,IGRPLT,BGFLAG,IVOL,IVOLOLD,ITIMBOV,
     X        ITIMEOV,AZVOL,ELVOL,NBMAX,NBVOL,NFXVOL,IEOS,IEOV,
     X        IEOF,NAMNEX,NFL_NEX,NEWDAY)
         CALL FLDIDNEX(NAMFLD,NFLDS,NAMNEX,NFL_NEX,IFLD)

      ELSE IF (IFMT.EQ.'DORADE') THEN
         CALL RDDO(IUN,IPREC,FRSTREC,ZSTR,PLTSW,COLRFIL,
     X        VECTS,NFRAME,IFD,NDUMP,NRST,IBSWEP,IESWEP,TANGMX,
     X        ANGINP,IRW,NEWDAY,IVOL,IVOLOLD,
     X        ITIMBOV,ITIMEOV,AZVOL,ELVOL,NBMAX,NBVOL,NFXVOL,
     X        IEOF,RADAR_TYPE,JSTAT)
         print *,'PPI_MMM: returned from RDDO'

      ELSE
         CALL RDFF(IUN,IPREC,FRSTREC,ZSTR,PLTSW,COLRFIL,
     X        VECTS,NFRAME,IFD,NDUMP,NRST,ITRANS,IBSWEP,IESWEP,
     X        TANGMX,ANGINP,XHTHR,XVTHR,IFORBLK,NEWDAY,IVOL,IVOLOLD,
     X        ITIMBOV,ITIMEOV,AZVOL,ELVOL,NBMAX,NBVOL,NFXVOL,
     X        IEOV,IEOF,JHSK,IHSK,RNGCOR,DEC,DECWR,WORDSZ)
      END IF

C     IF A SWEEP HAS NOT BEEN READ OR IF IT'S SHORT, DO NOT PROCESS.
C
      IF(ITPOLD.LT.1 .OR. ITPOLD.GT.8 .OR. NANG(1).LT.MNANG)THEN
         IF(IEOT.EQ.1)THEN
            ITPOLD=ITP
            GO TO 5
         END IF
         NSHORT=NSHORT+1
         GO TO 824
      END IF
      NSHORT=0

C     Increment the counter (MSCAN) for number of sweeps that involve
C     any kind of swath (nswth.ge.1) or accumulation operation.
C     Also, generate times and fixed angles for these operations.
C        IFTIME,ITIME  - Beginning, Ending times of scan (data.inc)
C        ITIME1,ITIME2 - Beginning, Ending times of the swath (swth.inc)
C        FXANG1,FXANG2 - Beginning, Ending fixed angles for swath (swth.inc)C
C
C     FMIN1 - MINUTES FROM BEGINNING TIME OF INTEGRATION
C             CONVERT FIRST SWEEP BEGINNING TIME (IFTIME - HHMMSS)
C     FMIN  - MINUTES FOR CURRENT INTEGRATION ENDING TIME 
C             CONVERT CURRENT SWEEP ENDING TIME (ITIME - HHMMSS)
C     FMIN2 - MINUTES FOR CURRENT INTEGRATION BEGINNING TIME
C             CONVERT PREVIOUS SWEEP ENDING TIME (ITOLD - HHMMSS)

      IF(NSWTH.GE.1)THEN
         MSCAN=MSCAN+1
         JMOD=MOD(MSCAN,NSWPAVG)
         IF(MSCAN.EQ.1.OR.JMOD.EQ.1)THEN
            ITIME1=IFTIME
            ITIME2=ITIME
            ITOLD=ITIME2
            IHR1= ITIME1/10000
            IMN1=(ITIME1-IHR1*10000)/100
            ISC1= ITIME1-IHR1*10000-IMN1*100
            FMIN1=FLOAT(IHR1*60+IMN1)+FLOAT(ISC1)/60.0
            IHR2= ITOLD/10000
            IMN2=(ITOLD-IHR2*10000)/100
            ISC2= ITOLD-IHR2*10000-IMN2*100
            FMIN2=FLOAT(IHR2*60+IMN2)+FLOAT(ISC2)/60.0
            FXANG1=FXOLD
            FXANG2=FXOLD
            MSCANOLD=MSCAN
         ELSE
            IHR2= ITOLD/10000
            IMN2=(ITOLD-IHR2*10000)/100
            ISC2= ITOLD-IHR2*10000-IMN2*100
            FMIN2=FLOAT(IHR2*60+IMN2)+FLOAT(ISC2)/60.0
            IHRM=IHR
            IF((IHRM-IHR2).LT.0)IHRM=IHRM+24
            FMIN2=FLOAT(IHRM*60+IMN2)+FLOAT(ISC2)/60.0
            FXANG2=FXOLD
            ITIME2=ITIME
         END IF
      END IF
c      print *,'PPI_MMM: mscan,mscanold,nswpavg,jmod=',
c     +     mscan,mscanold,nswpavg,jmod
c      print *,'PPI_MMM: itim1,iftim,itim,itim2=',
c     +     itime1,iftime,itime,itime2
c      print *,'PPI_MMM:    fxang1,fxang,fxang2=',fxang1,fxang,fxang2
C
C     CALL FUNCTIONS - CALCULATE ALL DERIVED FIELD (NF.GE.1)
C
      NFXVOL=NFXVOL+1
      IF(NFXVOL.LE.NFXMAX)THEN
         FXVOL(NFXVOL)=FXOLD
      END IF
c      print *,'Before CALL FUNC: nfxvol,fxvol=',nfxvol,fxvol(nfxvol)
c      print *,'Before CALL FUNC: nf=',nf,olat,olon,angxax
      IF(NF.GE.1)CALL FUNC(SFUN,MXNF,NF,NAMFLD,MNGATE,MXGATE,AZROT,
     X     ISW,AVGI,GXMIN,GXMAX,GYMIN,GYMAX,IAZC,CTDBXV,IFLD,
     X     NSWPAVG,SETGRD,XACT,YACT,ZACT,UACT,VACT,WACT,CACT,QACT,
     X     DACT,TACT,DTAC,IACT,MXL,NMRK,XMRK,YMRK,ZMRK,MXK,IMRK,
     X     OLAT,OLON,ANGXAX)

c-----do mmm=1,100
c--------print *,'aftfun:mscan,fxold,iw,nw,mmm=',
c     +        mscan,fxold,iwin-1,nwin,mmm
c-----end do
c-----print *,'aftfun: call sflush'
      call sflush

      IF(NP.GE.1)THEN

C     DRAW ALL CONTOUR PLOTS (NP.GE.1), 
C     INCLUDING SCAN-VOLUME ACCUMULATED SWATHS
C
         PLTSW=.FALSE.

         MP=0
         WRITE(6,8190)NFRAME+1
 8190    FORMAT('  CONTOURS: Starting frame =',I6)
 814     MP=MP+1
c         print *,'np,mp=',np,mp
         DO J=1,10
            JNDAT(J)=SINDAT(J,MP)
         END DO
c         print *,'   ',jndat(1),jndat(2),jndat(3),jndat(4),jndat(7)

C        SET THE BACKGROUND, FOREGROUND COLORS, OR THE NUMBER OF PLOTS PER FRAME,
C        OR DRAW ANY SWATHS ACCUMULATED OVER A SCAN VOLUME.
C
         IF (JNDAT(1).EQ.'LABELS ')THEN
            READ(JNDAT,815)LABLS
 815        FORMAT(/A3)
            IF(LABLS.NE.'NON'.AND.
     X         LABLS.NE.'ABR'.AND.
     X         LABLS.NE.'ALL')LABLS='ALL'
c            print *,'contour labels: ',labls
            GO TO 820
         ELSE IF (JNDAT(1).EQ.'BCKGRND ')THEN
            READ(JNDAT,817)GSTR,GRAYEST
 817        FORMAT(//F8.0/F8.0)
            IF(GSTR .LE. 0.0)GSTR=0.95
            IF(GRAYEST .GE. 1.0 .OR. GRAYEST .LT. 0.0)GRAYEST=0.0
c
c     This chunk was always executing when the BCKGRND
c     command was included in the "PLOTS" stack (NP.GE.1)
c     This meant that my SETBCKGRND calls were always being
c     overridden except when BCKGRND = WHITE
c
c            IF(JNDAT(2).EQ.'WHITE   ')THEN
c               BGFLAG='W'
c               IGRPLT=1
c               CALL GSCR(1,0,1.,1.,1.)
c               CALL GSCR(1,1,0.,0.,0.)
c            ELSE
c               BGFLAG=' '
c               IGRPLT=0
c               CALL GSCR(1,0,0.,0.,0.)
c               CALL GSCR(1,1,1.,1.,1.)
c            END IF
            BGFLAG=JNDAT(2)(1:1)
            CALL SETBCKGRND(BGFLAG)
            IF(BGFLAG.EQ.'B')THEN
               IGRPLT=0
            ELSE
               IGRPLT=1
            ENDIF

            CALL GSPLCI(1)
            CALL GSTXCI(1)
c-----------print *,'BCKGRND: Bg,gr=',bgflag,igrplt
c            print *,'  bckgrnd: flag,plt,gstr,gray=',bgflag,igrplt,
c     +           gstr,grayest
            GO TO 820

         ELSE IF(JNDAT(1).EQ.'SETWIN  ')THEN
            CALL SETWIN(JNDAT,NROW,NCOL,NWIN,XRT,YTP,SIDEX,SIDEY,IWIN,
     X           LABX,LABY,ILFLG,XBTP,YBTP,SIZBX)
            print *,'   setwin: fr,i,iw,nw=',nframe,i,iwin-1,nwin
            print *,'Finished with SETWIN'
            GO TO 820
         END IF

         IF(JNDAT(1).EQ.'VECTOR'.OR.JNDAT(1).EQ.'GRADF')THEN
            VECTS=.TRUE.
            CALL VECTORS(JNDAT,NAMFLD,IFLU,IFLW,IRSKIP,IASKIP,
     X                   VECSCL,USTRM,WSTRM,IOV,DROLD,COLRFIL,
     X                   PLTSW,IBSCAN,IVECCOL,MXF)
            CALL INIT1(ZSTR,COLRFIL,XRT(IWIN),YTP(IWIN),SIDEX(IWIN),
     X                 SIDEY(IWIN),IGRPLT,BGFLAG,LABX(IWIN),LABY(IWIN),
     X                 DXL,DXR,DYB,DYT,NROW,NCOL,XSHIFT,YSHIFT)
            IFL=IFLW
            IF(JNDAT(1).EQ.'VECTOR'.AND.ITPOLD.EQ.3)THEN
               XMN=XMIN(3)
               XMX=XMAX(3)
               ZMN=YMIN(3)
               ZMX=YMAX(3)
               CALL PLTVEC(DAT,IFLU,IFLW,IRSKIP,IASKIP,MNGATE,MXGATE,
     X                     NANG(1),BDVAL,XMN,XMX,ZMN,ZMX,USTRM,WSTRM,
     X                     VECSCL,AZA,H0,R0,DROLD,IVECCOL)
            ELSE
               XMN=XMIN(ITPOLD)
               XMX=XMAX(ITPOLD)
               YMN=YMIN(ITPOLD)
               YMX=YMAX(ITPOLD)
               CALL PLTGRD(DAT,IFLU,IFLW,IRSKIP,IASKIP,MNGATE,MXGATE,
     X                     NANG(1),BDVAL,XMN,XMX,YMN,YMX,USTRM,VSTRM,
     X                     VECSCL,AZA,ELA,X0,Y0,H0,R0,DR,IVECCOL,ITPOLD)
            END IF

         ELSE IF(JNDAT(1).EQ.'CONTOUR ' .OR. JNDAT(1).EQ.'CONTHIK ')THEN
            VECTS=.FALSE.
c            print *,'Before CONLEV: XmnXmxYmnYmx=',
c     +           XMIN(ITPOLD),XMAX(ITPOLD),YMIN(ITPOLD),YMAX(ITPOLD)
c            print *,'cont: iw,nw,msc=',iwin-1,nwin,mscan
c            print *,'  itpold,itpswa=',itpold,itpswa
            CALL CONLEV(JNDAT,NAMFLD,IFLD,IFL,ISW,COLRFIL,ICOLTYP,
     X                  ITERGT,CL,NL,CMIN,CMAX,CINC,ISHADE,IOV,
     X                  DRSW,DROLD,IBSCAN,PLTSW,IGRPLT,IGRYCON,
     X                  JMAP,JACT,JMRK,JNLD,JLMA,NOLAB,THIK,MXF,SINDAT,
     X                  MP,MXPLT,PROCESS,ITERBM,DIGMIN,DIGMAX,DIGOFF,
     X                  DIGCOLR,DIGSIZE,ROTATE,BGFLAG)
c            print *,'After CONLEV: XmnXmxYmnYmx=',
c     +           XMIN(ITPOLD),XMAX(ITPOLD),YMIN(ITPOLD),YMAX(ITPOLD)
            CALL INIT1(ZSTR,COLRFIL,XRT(IWIN),YTP(IWIN),SIDEX(IWIN),
     X                 SIDEY(IWIN),IGRPLT,BGFLAG,LABX(IWIN),LABY(IWIN),
     X                 DXL,DXR,DYB,DYT,NROW,NCOL,XSHIFT,YSHIFT)

C           When doing INTEGR, AVRAGE, OR STATS, compute outputs 
C           through current sweep from the accumulator arrays,
C
            AVNAM=NAMFLD(IFL)
            IF(IFLD(IFL).EQ.-5 .OR. 
     X         IFLD(IFL).EQ.-6 .OR.
     X         AVNAM(5:8).EQ.'mean' .OR. 
     X         AVNAM(5:8).EQ.'sdev')THEN
               CALL AVRAGE(DAT,MXR,MXA,MXF,BDVAL,MNGATE,MXGATE,NANG,
     X              NAMFLD,IFLD,IFL,AVNAM)
            END IF

            IF(IGRPLT.NE.IOLDGY.AND.ILOV.NE.1)THEN
               IOLDGY=IGRPLT
               CALL SETCOL(IGRPLT,BGFLAG,GRAYTYP,GSTR,GRAYEST)
            END IF
            IF(IOLDGY.EQ.1.AND.ILOV.EQ.1)IGRPLT=1

C           Set fill area interior style.
C           produce color fill
C
c            print *,'PPI_MMM: icoltyp=',icoltyp
            IF (ICOLTYP.EQ.'LQCOL1 '.OR.
     X          ICOLTYP.EQ.'LQCOL2 '.OR.
     X          ICOLTYP.EQ.'LQGRAY1'.OR.
     X          ICOLTYP.EQ.'LQGRAY2')THEN
               CALL GSFAIS(1)
               CALL CONFILL1(IGRPLT,IGRYCON,PLTSW,DELAMX)
c               print *,'PPI_MMM: after confill1'
            ELSE IF (ICOLTYP.EQ.'HQCOL1 '.OR.
     X               ICOLTYP.EQ.'HQCOL2 '.OR.
     X               ICOLTYP.EQ.'HQGRAY1'.OR.
     X               ICOLTYP.EQ.'HQGRAY2')THEN
               CALL GSFAIS(1)
               CALL CONFILL2(IGRPLT,IGRYCON,PLTSW,DELAMX)
c               print *,'PPI_MMM: after confill2'
            ELSE IF (ICOLTYP.EQ.'SAMPLOC ')THEN
               IFL=0
               VECTS=.TRUE.
               call sflush
c               print *,'samploc: nam,ifl,vects=',jndat(2),ifl,vects
c               print *,'samploc: icoltyp,digcolr,digsize=',
c     +              icoltyp,digcolr,digsize
               X_QUANT=DIGMIN
               Y_QUANT=DIGMAX
               A_QUANT=DIGOFF
               CALL PLT_RGLOC(ICOLTYP,DIGCOLR,DIGSIZE,ROTATE,X_QUANT,
     X                        Y_QUANT,A_QUANT)
            ELSE IF (ICOLTYP(1:4).EQ.'DIGT')THEN
               VECTS=.TRUE.
               call sflush
c               print *,'digitize: nam,ifl,vects=',jndat(2),ifl,vects
c               print *,'digitize: icoltyp,digcolr,digsize=',
c     +              icoltyp,digcolr,digsize
               CALL PLT_RGLOC(ICOLTYP,DIGCOLR,DIGSIZE,ROTATE,X_QUANT,
     X                        Y_QUANT,A_QUANT)
            ELSE
               IF(JNDAT(1)(5:7).EQ.'HIK')THEN
                  JLW=2000
               ELSE
                  JLW=1000*THIK
               ENDIF
               CALL CONTUR(IGRPLT,JLW,DELAMX)
            END IF

C           When doing INTEGR, AVRAGE, OR STATS, restore accumulators.
C     
            IF(IFLD(IFL).EQ.-5 .OR.
     X         IFLD(IFL).EQ.-6 .OR.
     X         AVNAM(5:8).EQ.'mean' .OR.
     X         AVNAM(5:8).EQ.'sdev')THEN
               CALL UNAVRAGE(DAT,MXR,MXA,MXF,BDVAL,MNGATE,MXGATE,NANG,
     X              NAMFLD,IFLD,IFL,AVNAM)
         END IF

         END IF

         XMN=GXMIN(ITPOLD)
         XMX=GXMAX(ITPOLD)
         YMN=GYMIN(ITPOLD)
         YMX=GYMAX(ITPOLD)

C        PLOT POLITICAL MAP WITHIN FRACTIONAL COORDINATES (FL,FR,FB,FT), THEN
C        CALL SET TO MAP USER COORDINATES (KM) BACK INTO FRACTIONAL ONES.
C        JMAP = (0) NO MAP, (1) SOLID LINES, (2) DOTTED LINES
C        In MAPSTC: 'CO' - continental outlines only, 'US' - US state outlines
C                   'PS' - continental, US state, and international outlines,
C                   'PO' - continental and international outlines
C
         IF(JMAP.GE.1)THEN
            CALL XY2LLDRV(BLLAT,BLLON,XMN,YMN,OLAT,OLON,ANGXAX)
            CALL XY2LLDRV(TRLAT,TRLON,XMX,YMX,OLAT,OLON,ANGXAX)
            CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
c-----------print *,'Bg,gr=',bgflag,igrplt
            IF(BGFLAG.EQ.'W')THEN
               CALL MAPSTI('C6',IBLACK)
            END IF
            IF(BGFLAG.EQ.' ')THEN
               CALL MAPSTI('C6',IWHITE)
            END IF
            CALL MAPSTC('OU','PS')
            IF(JMAP.EQ.1)THEN
               CALL MAPSTL('DO',.FALSE.)
            ELSE IF (JMAP.EQ.2)THEN
               CALL MAPSTL('DO',.TRUE.)
            ELSE
               CALL MAPSTL('DO',.TRUE.)
               CALL MAPSTI('DD',16)
            END IF
c            CALL MAPROJ('CE',MLAT,MLON,(90.0-ANGXAX))
            CALL MAPROJ('ME',MLAT,MLON,(90.0-ANGXAX))
            CALL MAPSET('CO',BLLAT,BLLON,TRLAT,TRLON)
            CALL MAPPOS(FL,FR,FB,FT)
            CALL MAPINT
            CALL MAPLOT
            CALL SET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
         END IF

         IF(IBOX.EQ.1)CALL PLOTBOX(X1BOX,Y1BOX,X2BOX,Y2BOX,WIDTH,NAREA)

C        PLOT MESONETWORK POSITIONS RELATIVE TO ORIGIN OF PLOT WINDOW
C
         IF(JMRK.GE.1.AND.IMRK.GT.0)THEN
            CALL PLTMRK(NMRK,XMRK,YMRK,ZMRK,AMRK,IMRK,NET,NNET,SMRK,
     X           XMN,XMX,YMN,YMX,JMRK,IGRPLT,BGFLAG,OLAT,OLON,ANGXAX,
     X           CMRK,PLT_NMRK,PLT_TIME,REC_DATA,MXK,MXT,KMRK,HCIRC,
     X           NEX_DATE,HOUR_BEG,HOUR_END,NHOURS,PLTCLOCK,PERCENT,
     X           PLTPERCNT)
         END IF

C        PLOT AIRCRAFT TRACK RELATIVE TO ORIGIN OF PLOT WINDOW
C
         IF(JACT.GE.1.AND.IACT.GT.0)THEN
            CALL PLTACT(XACT,YACT,ZACT,UACT,VACT,WACT,CACT,QACT,DACT,
     X           HACT,TACT,DTAC,DPAC,IACT,JACT,XMN,XMX,YMN,YMX,IFTIME,
     X           ITIME,X0,Y0,H0,FXOLD,ITPOLD,IGRPLT,BGFLAG,TMJR,TMNR,
     X           TS_LL,TS_SIZ,VECSCL,SCLR,WTYM,WMN,WMX,MXL,LABLS,
     X           IBLACK,IWHITE,NWIN,FXANG1,FXANG2,MXR,MXA,MNGATE,
     X           MXGATE,NANG(1),AZA,ELA,R0,DROLD)
         END IF

C        PLOT LMA LIGHTNING CHANNEL POSITIONS RELATIVE TO ORIGIN 
C        OF PLOT WINDOW
C
         IF(JLMA.GE.1.AND.ILMA.GT.0)THEN
            CALL PLTLMA(XLMA,YLMA,ZLMA,TLMA,HLMA,AZLMA,ILMA,JLMA,
     X           MXLM,DTLMA,XMN,XMX,YMN,YMX,IFTIME,ITIME,IGRPLT,
     X           BGFLAG,OLAT,OLON,ANGXAX,CLMA,FXOLD,HO,ITPOLD,
     X           DZLMA,NAMFLD(IFL),LMA_COLR)
         END IF

C        PLOT NLDN CG LIGHTNING STRIKE POSITIONS RELATIVE TO ORIGIN 
C        OF PLOT WINDOW
C
         IF(JNLD.GE.1.AND.INLD.GT.0)THEN
            CALL PLTNLD(XNLD,YNLD,PNLD,TNLD,HNLD,AZNLD,INLD,JNLD,
     X           MXNL,DTNLD,XMN,XMX,YMN,YMX,IFTIME,ITIME,IGRPLT,
     X           BGFLAG,OLAT,OLON,ANGXAX,CNLD,FXOLD,H0,ITPOLD,
     X           DZNLD,NAMFLD(IFL),IFLD(IFL),ITIME1)
         END IF

C        PUT LABELING ON PER-SWEEP PLOTS
C
         IF(NWIN.LE.1)THEN
            CALL LABEL(ZSTR,COLRFIL,PLTSW,VECTS,NFRAME,LABLS,IGRPLT,
     x                 BGFLAG,NOLAB,ICOLTYP)
         ELSE
            CALL LABELWIN(ZSTR,COLRFIL,PLTSW,VECTS,NFRAME,LABLS,IGRPLT,
     X                    BGFLAG,ICOLTYP)
         END IF

         IF(IOV.NE.1.AND.NWIN.GT.1)THEN
            IWIN=MOD(IWIN,NWIN+1)+1
            IF(IWIN.EQ.1)IWIN=2
c            print *,'main:     iwin,nwin=',iwin-1,nwin
         END IF
         ILOV=IOV
  820    IF(MP.LT.NP)GO TO 814
         PLTLAST='SWEEP'

C     END OF ALL CONTOUR PLOTS
     
      END IF

      IF(NWIN.GT.1 .AND. IWIN.GT.2)THEN
         IF(NAH.GE.1 .OR. NAS.GE.1 .OR. NVD.GE.1 .OR. 
     X      NRP.GE.1 .OR. NAP.GE.1 .OR. NSP.GE.1 .OR. 
     X      NSCAN.GE.1 )THEN

C     FINISH ANY OUTSTANDING MULTIPLE-PLOT FRAMES BEFORE 
C     PROCEEDING WITH OTHER TYPES OF PLOTS
C
            IOV=0
            IWIN=999
            CALL LABELWIN(ZSTR,COLRFIL,PLTSW,VECTS,NFRAME,
     X           LABLS,IGRPLT,BGFLAG,ICOLTYP)
            IWIN=2
            PLTLAST='SWEEP'
         END IF
      END IF

C     LIST ALL REQUESTED DATA FIELDS (NLST.GE.1)
C
      IF(NLST.GE.1)THEN
         CALL LISTFLD(NAMFLD,NFLDS,PRMNL,PRMXL,PAMNL,PAMXL,
     X        RSKIPL,ASKIPL,LSTNAM,NAMLST,NLST,PZMNL,PZMXL,H0)
      END IF

C     DRAW ALL HISTOGRAMS (NAH.GE.1)
C     If accumlated histograms are to be plotted after each sweep,
C     turn on here and these will not be done at end-of-volume.
C
      IF(NAH.GE.1)THEN
         AHST    = .FALSE.
         PLTEOV  = .FALSE.
         AHSTCLR = .FALSE.
         WRITE(6,8191)NFRAME+1
 8191    FORMAT(' PLTHIST: Starting frame =',I6)
         NFXHST=NFXHST+1
         IF(WHICH_AHST.EQ.'SWP')THEN
            AHST=.TRUE.
            IF(NSWPAVG.NE.999999 .AND. NFXHST.EQ.NSWPAVG)AHSTCLR=.TRUE.
            IF(NSWPAVG.EQ.999999 .AND. IEOV.EQ.1)AHSTCLR=.TRUE.
         END IF
         CALL PLTHIST(NAH,IHNAM,HRMN,HRMX,HAMN,HAMX,
     X        HZMN,HZMX,X0,Y0,H0,IHKP,JHKP,FMN,FMX,FBIN,FREF,
     X        PMN,PMX,HTYP,HFIT,PLTSW,NFRAME,AHST,AHSTCLR,
     X        HMATCH,NFXHST,NSWPAVG,PLTEOV,BGFLAG,HSTCOLR,LABLS)
         IF(AHSTCLR)THEN
            AHSTCLR=.FALSE.
            NFXHST=0
         END IF
      END IF

C     DRAW ALL SCATTERGRAMS (NAS.GE.1)
C     If accumlated scattergrams are to be plotted after each sweep,
C     turn on here and these will not be done at end-of-volume.
C
      IF(NAS.GE.1)THEN
         ASCT    = .FALSE.
         PLTEOV  = .FALSE.
         ASCTCLR = .FALSE.
         WRITE(6,8192)NFRAME+1
 8192    FORMAT(' PLTSCAT: Starting frame =',I6)
         NFXSCT=NFXSCT+1
         CALL PLTSCAT(NAS,SRMN,SRMX,SAMN,SAMX,SZMN,SZMX,X0,Y0,H0,
     X        ISKP,JSKP,IXNAM,XFMN,XFMX,IYNAM,YFMN,YFMX,SLOP,YCEPT,
     X        RLAG,ALAG,NSMX,PLTSW,NFRAME,ASCT,ASCTCLR,XDAT,YDAT,
     X        NDAT,NDMX,MSKP,NSKP,SMATCH,NFXSCT,NSWPAVG,PLTEOV,
     X        BGFLAG,SCTCOLR,LABLS)

         IF(WHICH_ASCT.EQ.'SWP')THEN
            ASCT=.TRUE.
            IF(NSWPAVG.NE.999999 .AND. NFXSCT.EQ.NSWPAVG)ASCTCLR=.TRUE.
            IF(NSWPAVG.EQ.999999 .AND. IEOV.EQ.1)ASCTCLR=.TRUE.
            CALL PLTSCAT(NAS,SRMN,SRMX,SAMN,SAMX,SZMN,SZMX,X0,Y0,H0,
     X           ISKP,JSKP,IXNAM,XFMN,XFMX,IYNAM,YFMN,YFMX,SLOP,YCEPT,
     X           RLAG,ALAG,NSMX,PLTSW,NFRAME,ASCT,ASCTCLR,XDAT,YDAT,
     X           NDAT,NDMX,MSKP,NSKP,SMATCH,NFXSCT,NSWPAVG,PLTEOV,
     X           BGFLAG,SCTCOLR,LABLS)
            IF(ASCTCLR)THEN
               ASCTCLR=.FALSE.
               NFXSCT=0
            END IF
         END IF

      END IF

C     DRAW ALL VAD SCATTERGRAMS (JVD.GE.1)
C
      IF(JVD.GE.1)THEN
         WRITE(6,8193)NFRAME+1
 8193    FORMAT('    PLTVAD: Starting frame =',I6)
         CALL PLTVAD(NVD,JVD,ZMNVD,ZMXVD,ISKPVD,WFILT,XMNVD,XMXVD,
     X        XSCLVD,XREFVD,TYPVD,H0,PLTSW,NFRAME,LABLS,BGFLAG)
      END IF

C     DRAW ALL CONSTANT ANGLE PLOTS (NRP.GE.1)
C
      IF(NRP.GE.1)THEN
        WRITE(6,8194)NFRAME+1
 8194   FORMAT('   PLTRNGE: Starting frame =',I6)
         CALL PLTRNGE(NAMFLD,NFLDS,PRMNR,PRMXR,PAMNR,PAMXR,ASKIP,
     X                IRNAM,RFMN,RFMX,RREF,RPRNT,NRP,COLRFIL,PLTSW,
     X                NFRAME,IAZC,IFLD,BGFLAG,LTYP)
      END IF

C     DRAW ALL CONSTANT RANGE PLOTS (NAP.GE.1)
C
      IF(NAP.GE.1)THEN
         WRITE(6,8195)NFRAME+1
 8195    FORMAT('   PLTANGL: Starting frame =',I6)
         CALL PLTANGL(NAMFLD,NFLDS,PAMNA,PAMXA,PRMNA,PRMXA,RSKIP,
     X                IANAM,AFMN,AFMX,AREF,APROC,ACNT,AGAP,AERR,
     X                APRNT,NAP,COLRFIL,PLTSW,NFRAME,H0,IFLD,
     X                BGFLAG,LTYP)
      END IF

C     DRAW ALL SPECTROGRAMS (NSP.GE.1)
C
      IF(NSP.GE.1)THEN
         CALL SETBCKGRND(BGFLAG)
         WRITE(6,8196)NFRAME+1
 8196    FORMAT('   PLTSPEC: Starting frame =',I6)
         CALL PLTSPEC(NSP,IPNAM,PRMN,PRMX,PAMN,PAMX,IPKP,JPKP,PTYP,
     X                SPAVG,FRQMN,FRQMX,PEXMN,PEXMX,FRQAX,AMPAX,
     X                DTREND,PTAVG,FLDMN,FLDMX,FLDRF,PLTSW,NFRAME,
     X                BGFLAG)
      END IF

C     DRAW ANGLE SCAN (NSCAN.EQ.1)
C
      IF(NSCAN.EQ.1)THEN
         WRITE(6,8197)NFRAME+1
 8197    FORMAT('   PLTSCAN: Starting frame =',I6)
         CALL PLTSCAN(AZROT,COLRFIL,PLTSW,NFRAME)
      END IF

C     WRITE DATA OUT IN UNIVERSAL FORMAT
C
      IF(IWRUF.EQ.1)CALL UFWRITE(DAT,ITPOLD,NAMOUT,NMOUTUF,NUMUF,
     X     IUFUN,NUMUN,IYR,IMON,IDAY,ITM,FXOLD,AZA,ELA,NANG,NAMFLD,
     X     NFLDS,R0,DROLD,NGTS,BDVAL,VNYQ,IWRUF,IWFOR,MXR,MXA,MXF,
     X     DEC,DECWR,WORDSZ,NETWORK,IRATYP,GRDTYP)

 824  CONTINUE

C     Set flags to do "SWATH" plots (NSWTH.GE.1) when conditions are met.
C        First turn off all "swath" plotting, then turn on 
C        when any one of the following conditions is met:
C        1) End of a volume scan and end of a file: NSWPAVG = 999999
C           Or after NSWPAVG scans: MOD(MSCAN,NSWPAVG) = 0
C        2) When any one of PLTPROJ, PLTAE, PLTHZ, PLTAHST, or PLTASCT 
C           requested.
C        3) Only if scan type (ISCTP) is one requested for swaths (SCANTYP).
C        4) Always at end of the run - ISTOP = 1
C     But only if some kind of swath has been requested (NSWTH .GE. 1),
C     and some requested scan types have been completed (MSCAN .GT. 0).
C
      PLTEOV=.FALSE.
c      print *,'PPI_MMM: plteov,nfxsct,nfxhst,mscan=',
c     +     plteov,nfxsct,nfxhst,mscan
c      IF(NSWTH.GE.1)THEN
c         IF(NFXSCT.GT.0 .AND. MSCAN.EQ.0)MSCAN=NFXSCT
c         IF(NFXHST.GT.0 .AND. MSCAN.EQ.0)MSCAN=NFXHST
c      END IF
c      IF(NSWPAVG.EQ.999999)PLTEOV=.TRUE.
      IF(IVOL.NE.IVOLOLD)PLTEOV=.TRUE.
      IF(IEOV.EQ.1)PLTEOV=.TRUE.
      IF(IEOF.EQ.1)PLTEOV=.TRUE.
      IF(IEOT.EQ.1)PLTEOV=.TRUE.
      IF(NSWPAVG.NE.999999)THEN
         JMOD=MOD(MSCAN,NSWPAVG)
         IF(JMOD.EQ.0)PLTEOV=.TRUE.
      END IF
C-----IF(ISCTP(ITPOLD).NE.SCANTYP)PLTEOV=.FALSE.
      IF(ISTOP.EQ.1)PLTEOV=.TRUE.
      IF(WHICH_AHST.EQ.'EOV' .AND. IEOV.EQ.1)PLTEOV=.TRUE.
      IF(WHICH_ASCT.EQ.'EOV' .AND. IEOV.EQ.1)PLTEOV=.TRUE.
c      print *,'PLTEOV: ivol,ivolold=',plteov,ivol,ivolold
c      print *,'PLTEOV:        flags=',ieov,ieof,ieot,jmod,istop,nswpavg
c      print *,'PLTEOV:    asct,ahst=',which_asct,which_ahst

c     Print statement for debugging swath plotting logic.
c
c-----debug (ljm)
c      if(nswth.ge.1)then
c         print *,'swath=',jndat(1),isctp(itpold),scantyp,
c     +        nswpavg,pltlast,nwin,iwin
c      end if
c-----debug (ljm)

C     Finish labels and CALL FRAME if any outstanding during-volume multiple
C     panel frames before proceeding with any end-of-volume swath plots.  
C     Requires new SETWIN (entry SETWIN2) and INIT1.  Decrement IWIN.
C     Caution:  Designed to allow several (NWIN) scans (times) onto same 
C     frame, and then calling frame for any partially filled frame.  
C     Only multiple-time CONTOUR and CNTSWTH allowed in this mode.
C
      IF(PLTLAST .EQ. 'SWEEP' .AND. PLTEOV .AND.
     X     (NWIN.GT.1 .AND. IWIN.GT.2))THEN
         IWIN=IWIN-1
         NWIN=IWIN-1
         CALL SETWIN2(JNDAT,NROW,NCOL,NWIN,XRT,YTP,SIDEX,SIDEY,IWIN,
     X        LABX,LABY,ILFLG,XBTP,YBTP,SIZBX)
         CALL INIT1(ZSTR,COLRFIL,XRT(IWIN),YTP(IWIN),SIDEX(IWIN),
     X        SIDEY(IWIN),IGRPLT,BGFLAG,LABX(IWIN),LABY(IWIN),
     X        DXL,DXR,DYB,DYT,NROW,NCOL,XSHIFT,YSHIFT)

C        VECTS=.TRUE. will bypass field and contour labels in LABELWIN
C        IOV=0 since only want to finish heading label and CALL MYFRAME.
C
         IOV=0
         VECTS=.TRUE.
         CALL LABELWIN(ZSTR,COLRFIL,PLTSW,VECTS,NFRAME,
     X        LABLS,IGRPLT,BGFLAG,ICOLTYP)
         VECTS=.FALSE.
         PLTLAST='SWEEP'
      END IF

c     Print statement for debugging swath plotting logic.
c
c-----debug (ljm)
c      if(nswth.ge.1)then
c         print *,'CntSw: nswth,mscan,plteov,pltlast=',
c     +        nswth,mscan,plteov,pltlast
c         write(6,1770)itimbov,itimeov,fxold,nshort,nbvol,nfxvol,
c     +        nfxhst,nfxsct,itpold,itp,ivolold,ivol,ieof,ieot,istop,
c     +        mscan,nswpavg,mod(mscan,nswpavg),isctp(itpold),scantyp
c1770    format('*cntsw:time,fx,flg=',i6.6,'-',i6.6,f6.1,13i4,i7,i4,
c     +        2a4)
c      end if
c-----debug (ljm)
      
      IF( NSWTH.GE.1 .AND. MSCAN .GT. 0 .AND. PLTEOV 
     X     .AND. PLTLAST .NE. 'ENDVOL')THEN
 
C     END-OF-VOLUME-SCAN: Execute swath or accumulation plotting
C        NSWTH   - number of swaths, including accumulations
C        MSCAN   - number of fixed-angle scans already read
C        PLTEOV  - true if new volume, eov, eof, eot, nswpavg, or
C                  STOP command read.
C        PLTLAST - ENDVOL, if haven't plotted swath or accumulations
C                  This is basically a wrapup flag to ensure that
C                  swath or accumulation plotting gets done at the
C                  end of a volume scan.
C
c-----debug (ljm)
c         write(6,1771)itimbov,itimeov,fxold,nshort,nbvol,nfxvol,
c     +        nfxhst,nfxsct,itpold,itp,ivolold,ivol,ieof,ieot,istop,
c     +        mscan,nswpavg,mod(mscan,nswpavg),isctp(itpold),scantyp
c 1771    format(1x,'cntsw:time,fx,flg=',i6.6,'-',i6.6,f6.1,13i4,i7,i4,
c     +        2a4)
c         print *,'pltasct: nfxsct,nswpavg=',nfxsct,nswpavg
c-----debug (ljm)

         PLTSW=.TRUE.
         VECTS=.FALSE.
c         IFTIME=ITIMBOV
c         ITIME=ITIMEOV
c         ITIME1=ITIMBOV
c         ITIME2=ITIMEOV

         DO 830 I=1,NSWTH
            DO J=1,10
               JNDAT(J)=SWTHDAT(J,I)
            END DO
            IF(I.EQ.1)THEN
               WRITE(6,825)NFRAME
 825           FORMAT(' VOLUME SCAN ENDING   FRAME =',I6,/)
               WRITE(6,826)NFRAME+1,JNDAT(1)
 826           FORMAT(' EOV PLTSWAT STARTING FRAME =',I6,2X,A8)
            END IF
            
            IF (JNDAT(1).EQ.'BCKSWTH ')THEN
               IF(JNDAT(2).EQ.'WHITE   ')THEN
                  BGFLAG='W'
                  IGRPLT=1
                  CALL GSCR(1,0,1.,1.,1.)
                  CALL GSCR(1,1,0.,0.,0.)
               ELSE
                  BGFLAG=' '
                  IGRPLT=0
                  CALL GSCR(1,0,0.,0.,0.)
                  CALL GSCR(1,1,1.,1.,1.)
               END IF
               CALL GSPLCI(1)
               CALL GSTXCI(1)
c               print *,'bcksw:  bg,igrplt=',bgflag,igrplt

            ELSE IF(JNDAT(1).EQ.'WINSWTH  ')THEN
               CALL SETWIN(JNDAT,NROW,NCOL,NWIN,XRT,YTP,SIDEX,SIDEY,
     X              IWIN,LABX,LABY,ILFLG,XBTP,YBTP,SIZBX)
c               print *,'winsw: fr,i,iw,nw=',nframe+1,i,iwin-1,nwi               GO TO 830

            ELSE IF(JNDAT(1).EQ.'PLTAE' .AND. NBVOL.GT.0)THEN
c               print *,'pltae: fr,i,iw,nw=',nframe+1,i,iwin-1,nwin
               CALL PLTAE(JNDAT,ITIMBOV,ITIMEOV,AZVOL,ELVOL,NBMAX,NBVOL,
     X              IDAY,IMON,IYR,NETWORK,IRATYP,ICORD,IVOLOLD,ITPOLD,
     X              NFRAME,FXVOL,NFXMAX,NFXVOL,BGFLAG)
               WRITE(6,827)NFRAME,ITIMBOV,ITIMEOV,NBVOL
 827           FORMAT('      PLTAE: FRAMES PLOTTED =',I6,
     +              ' TIME=',I6.6,'-',I6.6,'  NO. BEAMS=',I6)
c               NBVOL=0
c               NFXVOL=0

            ELSE IF(JNDAT(1).EQ.'PLTHZ' .AND. NBVOL.GT.0)THEN
c               print *,'plthz: fr,i,iw,nw=',nframe+1,i,iwin-1,nwin
               CALL PLTHZ(JNDAT,ITIMBOV,ITIMEOV,AZVOL,ELVOL,NBMAX,NBVOL,
     X              IDAY,IMON,IYR,NETWORK,IRATYP,ICORD,IVOLOLD,ITPOLD,
     X              NFRAME,FXVOL,NFXMAX,NFXVOL,BGFLAG,H0,R0,DROLD,
     X              MNGATE,MXGATE)
               WRITE(6,8271)NFRAME,ITIMBOV,ITIMEOV,NBVOL
 8271          FORMAT('      PLTHZ: FRAMES PLOTTED =',I6,
     +              ' TIME=',I6.6,'-',I6.6,'  NO. BEAMS=',I6)
c               NBVOL=0
c               NFXVOL=0

            ELSE IF(JNDAT(1).EQ.'PLTPROJ '.AND. NFXVOL.GT.0)THEN
c               print *,'pltproj: fr,i,iw,nw=',nframe+1,i,iwin-1,nwin
               CALL PLTPROJ(JNDAT,ITIMBOV,ITIMEOV,FXVOL,NFXVOL,NFXMAX,
     X              NETWORK,IRATYP,ICORD,IVOLOLD,NFRAME,JTP,JMAP,JACT,
     X              JMRK)

               XMN=GXMIN(JTP)
               XMX=GXMAX(JTP)
               YMN=GYMIN(JTP)
               YMX=GYMAX(JTP)
               write(6,1772)jtp,xmn,xmx,ymn,ymx,iftime,itime
 1772          format('pltact:',i8,4f8.1,2i8)

C              PLOT MESONETWORK POSITIONS RELATIVE TO ORIGIN OF PLOT WINDOW
C
               IF(JMRK.GE.1.AND.IMRK.GT.0)THEN
                  CALL PLTMRK(NMRK,XMRK,YMRK,ZMRK,AMRK,IMRK,NET,NNET,
     X                 SMRK,XMN,XMX,YMN,YMX,JMRK,IGRPLT,BGFLAG,OLAT,
     X                 OLON,ANGXAX,CMRK,PLT_NMRK,PLT_TIME,REC_DATA,MXK,
     X                 MXT,KMRK,HCIRC,NEX_DATE,HOUR_BEG,HOUR_END,NHOURS,
     X                 PLTCLOCK,PERCENT,PLTPERCNT)
               END IF

C              PLOT AIRCRAFT TRACK RELATIVE TO ORIGIN OF PLOT WINDOW
C              Input DTAC=-DTAC to force aircraft track to be plotted 
C              from the beginning (IFTIME-DTAC) to ending (ITIME+DTAC)
C              times of the volume scan
C
               IF(JACT.GE.1.AND.IACT.GT.0)THEN
                  SDTAC=-1.0*DTAC
                  HVECSCL=10.0*VECSCL
                  CALL PLTACT(XACT,YACT,ZACT,UACT,VACT,WACT,CACT,QACT,
     X                 DACT,HACT,TACT,SDTAC,DPAC,IACT,JACT,XMN,XMX,YMN,
     X                 YMX,IFTIME,ITIME,X0,Y0,H0,0.0,JTP,IGRPLT,BGFLAG,
     X                 TMJR,TMNR,TS_LL,TS_SIZ,HVECSCL,SCLR,WTYM,WMN,
     X                 WMX,MXL,LABLS,IBLACK,IWHITE,NWIN,FXANG1,FXANG2,
     X                 MXR,MXA,MNGATE,MXGATE,NANG(1),AZA,ELA,R0,DROLD)
               END IF

C        PLOT LMA LIGHTNING CHANNEL POSITIONS RELATIVE TO ORIGIN 
C        OF PLOT WINDOW
C
               IF(JLMA.GE.1.AND.ILMA.GT.0)THEN
                  CALL PLTLMA(XLMA,YLMA,ZLMA,TLMA,HLMA,AZLMA,ILMA,
     X                 JLMA,MXLM,DTLMA,XMN,XMX,YMN,YMX,IFTIME,ITIME,
     X                 IGRPLT,BGFLAG,OLAT,OLON,ANGXAX,CLMA,FXOLD,H0,
     X                 ITPOLD,DZLMA,NAMFLD(IFL),LMA_COLR)
               END IF

C              PLOT NLDN CG LIGHTNING STRIKE POSITIONS RELATIVE TO ORIGIN 
C              OF PLOT WINDOW
C
               IF(JNLD.GE.1.AND.INLD.GT.0)THEN
                  CALL PLTNLD(XNLD,YNLD,PNLD,TNLD,HNLD,AZNLD,INLD,JNLD,
     X                 MXNL,DTNLD,XMN,XMX,YMN,YMX,IFTIME,ITIME,IGRPLT,
     X                 BGFLAG,OLAT,OLON,ANGXAX,CNLD,FXOLD,H0,ITPOLD,
     X                 DZNLD,NAMFLD(IFL),IFLD(IFL),ITIME1)
               END IF

C              PLOT POLITICAL MAP WITHIN FRACTIONAL COORDINATES (FL,FR,FB,FT), 
C              THEN CALL SET TO MAP USER COORDINATES (KM) BACK INTO FRACTIONAL 
C              ONES. JMAP = (0) NO MAP, (1) SOLID LINES, (2) DOTTED LINES
C
               IF(JMAP.GE.1)THEN
                  CALL XY2LLDRV(BLLAT,BLLON,XMN,YMN,OLAT,OLON,ANGXAX)
                  CALL XY2LLDRV(TRLAT,TRLON,XMX,YMX,OLAT,OLON,ANGXAX)
                  CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
c-----------------print *,'Bg,gr=',bgflag,igrplt
                  IF(BGFLAG.EQ.'W')THEN
                     CALL MAPSTI('C6',IBLACK)
                  END IF
                  IF(BGFLAG.EQ.' ')THEN
                     CALL MAPSTI('C6',IWHITE)
                  END IF
                  CALL MAPSTC('OU','PS')
                  IF(JMAP.EQ.1)THEN
                     CALL MAPSTL('DO',.FALSE.)
                  ELSE IF (JMAP.EQ.2)THEN
                     CALL MAPSTL('DO',.TRUE.)
                  ELSE
                     CALL MAPSTL('DO',.TRUE.)
                     CALL MAPSTI('DD',16)
                  END IF
c                  CALL MAPROJ('CE',MLAT,MLON,(90.0-ANGXAX))
                  CALL MAPROJ('ME',MLAT,MLON,(90.0-ANGXAX))
                  CALL MAPSET('CO',BLLAT,BLLON,TRLAT,TRLON)
                  CALL MAPPOS(FL,FR,FB,FT)
                  CALL MAPINT
                  CALL MAPLOT
                  CALL SET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
               END IF

C              PUT LABELING ON END-OF-VOLUME PLOTS
C
               ITPSWA=JTP
               FXSWA=0.0
               CALL LABEL(ZSTR,COLRFIL,PLTSW,VECTS,NFRAME,LABLS,IGRPLT,
     x              BGFLAG,NOLAB,ICOLTYP)
               WRITE(6,828)NFRAME,ITIMBOV,ITIMEOV,NFXVOL
 828           FORMAT('    PLTPROJ: FRAMES PLOTTED =',I6,
     +              ' TIME=',I6.6,'-',I6.6,'  NO. FIXED ANGLES=',I6)
               NFXVOL=0
               ITPSWA=ITPOLD
               FXSWA=FXOLD

            ELSE IF(JNDAT(1).EQ.'CNTSWTH  '.AND. ISTOP.NE.1)THEN
c               print *,'cntsw: fr,i,iw,nw=',nframe+1,i,iwin-1,nwin
               FXOLD=FXVOL(NINT(0.5*NFXVOL))
               FXSWA=FXOLD
c               print *,'fxold=',fxold
               CALL PLTSWAT(JNDAT,ZSTR,PLTSW,VECTS,NFRAME,IGRPLT,
     X                      IGRYCON,JMAP,JACT,JMRK,JNLD,BGFLAG,SINDAT,
     X                      MP,MXPLT,DELAMX,PROCESS,NFXVOL,COLRFIL,
     X                      NOLAB,ICOLTYP,ROTATE,X_QUANT,Y_QUANT)

               IF(JNDAT(4).NE.'RESET   ')THEN

                  XMN=GXMIN(ITPOLD)
                  XMX=GXMAX(ITPOLD)
                  YMN=GYMIN(ITPOLD)
                  YMX=GYMAX(ITPOLD)
c                  write(6,1772)itpold,xmn,xmx,ymn,ymx,iftime,itime

C                 PLOT POLITICAL MAP WITHIN FRACTIONAL COORDINATES (FL,FR,FB,FT), 
C                 THEN CALL SET TO MAP USER COORDINATES (KM) BACK INTO FRACTIONAL 
C                 ONES. JMAP = (0) NO MAP, (1) SOLID LINES, (2) DOTTED LINES
C
                  IF(JMAP.GE.1)THEN
                     CALL XY2LLDRV(BLLAT,BLLON,XMN,YMN,OLAT,OLON,ANGXAX)
                     CALL XY2LLDRV(TRLAT,TRLON,XMX,YMX,OLAT,OLON,ANGXAX)
                     CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
c--------------------print *,'Bg,gr=',bgflag,igrplt
                     IF(BGFLAG.EQ.'W')THEN
                        CALL MAPSTI('C6',IBLACK)
                     END IF
                     IF(BGFLAG.EQ.' ')THEN
                        CALL MAPSTI('C6',IWHITE)
                     END IF
                     CALL MAPSTC('OU','PS')
                     IF(JMAP.EQ.1)THEN
                        CALL MAPSTL('DO',.FALSE.)
                     ELSE IF (JMAP.EQ.2)THEN
                        CALL MAPSTL('DO',.TRUE.)
                     ELSE
                        CALL MAPSTL('DO',.TRUE.)
                        CALL MAPSTI('DD',16)
                     END IF
c                     CALL MAPROJ('CE',MLAT,MLON,(90.0-ANGXAX))
                     CALL MAPROJ('ME',MLAT,MLON,(90.0-ANGXAX))
                     CALL MAPSET('CO',BLLAT,BLLON,TRLAT,TRLON)
                     CALL MAPPOS(FL,FR,FB,FT)
                     CALL MAPINT
                     CALL MAPLOT
                     CALL SET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
                  END IF
                  
C                 PLOT MESONETWORK POSITIONS RELATIVE TO ORIGIN OF PLOT WINDOW
C
                  IF(JMRK.GE.1.AND.IMRK.GT.0)THEN
                  CALL PLTMRK(NMRK,XMRK,YMRK,ZMRK,AMRK,IMRK,NET,NNET,
     X                 SMRK,XMN,XMX,YMN,YMX,JMRK,IGRPLT,BGFLAG,OLAT,
     X                 OLON,ANGXAX,CMRK,PLT_NMRK,PLT_TIME,REC_DATA,MXK,
     X                 MXT,KMRK,HCIRC,NEX_DATE,HOUR_BEG,HOUR_END,NHOURS,
     X                 PLTCLOCK,PERCENT,PLTPERCNT)
                  END IF

C                 PLOT AIRCRAFT TRACK RELATIVE TO ORIGIN OF PLOT WINDOW
C                 Input DTAC=-DTAC to force aircraft track to be plotted 
C                 from the beginning (IFTIME-DTAC) to ending (ITIME+DTAC)
C                 times of the volume scan
C
                  IF (JACT.GE.1.AND.IACT.GT.0)THEN
                     SDTAC=-1.0*DTAC
                     SDPAC=10.0*DPAC
                     CALL PLTACT(XACT,YACT,ZACT,UACT,VACT,WACT,CACT,
     X                    QACT,DACT,HACT,TACT,SDTAC,SDPAC,IACT,JACT,
     X                    XMN,XMX,YMN,YMX,IFTIME,ITIME,X0,Y0,H0,FXOLD,
     X                    ITPOLD,IGRPLT,BGFLAG,TMJR,TMNR,TS_LL,TS_SIZ,
     X                    VECSCL,SCLR,WTYM,WMN,WMX,MXL,LABLS,IBLACK,
     X                    IWHITE,NWIN,FXANG1,FXANG2,MXR,MXA,MNGATE,
     X                    MXGATE,NANG(1),AZA,ELA,R0,DROLD)
                  END IF
                  
C     PLOT LMA LIGHTNING CHANNEL POSITIONS RELATIVE TO ORIGIN 
C     OF PLOT WINDOW
C
                  IF(JLMA.GE.1.AND.ILMA.GT.0)THEN
                     CALL PLTLMA(XLMA,YLMA,ZLMA,TLMA,HLMA,AZLMA,ILMA,
     X                    JLMA,MXLM,DTLMA,XMN,XMX,YMN,YMX,IFTIME,ITIME,
     X                    IGRPLT,BGFLAG,OLAT,OLON,ANGXAX,CLMA,FXOLD,H0,
     X                    ITPOLD,DZLMA,NAMFLD(IFL),LMA_COLR)
                  END IF

C     PLOT NLDN CG LIGHTNING STRIKE POSITIONS RELATIVE TO ORIGIN 
C     OF PLOT WINDOW
C     
                  IF(JNLD.GE.1.AND.INLD.GT.0)THEN
                     CALL PLTNLD(XNLD,YNLD,PNLD,TNLD,HNLD,AZNLD,INLD,
     X                    JNLD,MXNL,DTNLD,XMN,XMX,YMN,YMX,IFTIME,ITIME,
     X                    IGRPLT,BGFLAG,OLAT,OLON,ANGXAX,CNLD,FXOLD,H0,
     X                    ITPOLD,DZNLD,NAMFLD(IFL),IFLD(IFL),ITIME1)
                  END IF


C                 DO LABELS and CALL FRAME
C
                  IF(NWIN.LE.1)THEN
                     CALL LABEL(ZSTR,COLRFIL,PLTSW,VECTS,NFRAME,
     X                    LABLS,IGRPLT,BGFLAG,NOLAB,ICOLTYP)
                  ELSE
                     CALL LABELWIN(ZSTR,COLRFIL,PLTSW,VECTS,NFRAME,
     X                    LABLS,IGRPLT,BGFLAG,ICOLTYP)
                  END IF
                  IF(IOV.NE.1.AND.NWIN.GT.1)THEN
                     IWIN=MOD(IWIN,NWIN+1)+1
                     IF(IWIN.EQ.1)IWIN=2
                  END IF
               END IF

            ELSE IF(JNDAT(1).EQ.'PLTAHST' .AND. NFXHST.GT.0)THEN
               IF(WHICH_AHST.EQ.'EOV')THEN

C                 Turn on end-of-volume accumulated histograms
C
                  PLTEOV  = .TRUE.
                  AHST    = .FALSE.
                  AHSTCLR = .FALSE.
                  IF(JNDAT(2).EQ.'CLRBINS'.AND. 
     X               (NFXHST.EQ.NSWPAVG .OR. NSWPAVG.EQ.999999))THEN
                     AHSTCLR=.TRUE.
                  END IF
c                  print *,'pltahst: fr,i,iw,nw=',nframe+1,i,iwin-1,nwin
c                  print *,'PLTAHST: clr,nfx,navg=',ahstclr,nfxhst,
c     +                 nswpavg
                  CALL PLTHIST(NAH,IHNAM,HRMN,HRMX,HAMN,HAMX,
     X                 HZMN,HZMX,X0,Y0,H0,IHKP,JHKP,FMN,FMX,FBIN,FREF,
     X                 PMN,PMX,HTYP,HFIT,PLTSW,NFRAME,AHST,AHSTCLR,
     X                 HMATCH,NFXHST,NSWPAVG,PLTEOV,BGFLAG,HSTCOLR,
     X                 LABLS)
                  WRITE(6,829)NFRAME,ITIMBOV,ITIMEOV,NFXHST
 829              FORMAT('    PLTAHST: FRAMES PLOTTED =',I6,
     +                 ' TIME=',I6.6,'-',I6.6,'  NO. FIXED ANGLES=',I6)
                  IF(AHSTCLR)THEN
                     NFXHST=0
                     AHSTCLR=.FALSE.
                  END IF
               END IF

            ELSE IF(JNDAT(1).EQ.'PLTASCT' .AND. NFXSCT.GT.0)THEN
               IF(WHICH_ASCT.EQ.'EOV')THEN

C                 Turn on end-of-volume accumulated scattergrams
C
                  PLTEOV  = .TRUE.
                  ASCT    = .FALSE.
                  ASCTCLR = .FALSE.
                  IF(JNDAT(2).EQ.'CLRBINS'.AND. 
     X               (NFXSCT.EQ.NSWPAVG .OR. NSWPAVG.EQ.999999))THEN
                     ASCTCLR=.TRUE.
                  END IF
c                  print *,'pltasct: fr,i,iw,nw=',nframe+1,i,iwin-1,nwin
c                  print *,'PLTASCT: clr,nfx,navg=',asctclr,nfxsct,
c     +                 nswpavg
                  CALL PLTSCAT(NAS,SRMN,SRMX,SAMN,SAMX,SZMN,SZMX,X0,Y0,
     X                 H0,ISKP,JSKP,IXNAM,XFMN,XFMX,IYNAM,YFMN,YFMX,
     X                 SLOP,YCEPT,RLAG,ALAG,NSMX,PLTSW,NFRAME,ASCT,
     X                 ASCTCLR,XDAT,YDAT,NDAT,NDMX,MSKP,NSKP,SMATCH,
     X                 NFXSCT,NSWPAVG,PLTEOV,BGFLAG,SCTCOLR,LABLS)
                  WRITE(6,8291)NFRAME,ITIMBOV,ITIMEOV,NFXSCT
 8291             FORMAT(1X,' PLTASCT: FRAMES PLOTTED =',I6,
     +                 ' TIME=',I6.6,'-',I6.6,'  NO. FIXED ANGLES=',I6)
                  IF(ASCTCLR)THEN
                     NFXSCT=0
                     ASCTCLR=.FALSE.
                  END IF
               END IF
            END IF

 830     CONTINUE
         PLTLAST='ENDVOL'
         WRITE(6,831)NFRAME,JNDAT(1)
 831     FORMAT(' EOV PLTSWAT   ENDING FRAME =',I6,2X,A8)
         PLTSW=.FALSE.
         IF(NSWPAVG.NE.999999)THEN
            JMOD=MOD(MSCAN,NSWPAVG)
            IF(JMOD.EQ.0)MSCAN=0
         END IF
         IF(NSWPAVG.EQ.999999 .AND. IEOV.EQ.1)MSCAN=0
         IF(ISTOP.EQ.1)GO TO 900
      END IF

 834  CONTINUE

C     PRINT VOLUME HEADER AND LOOP TO NEXT SCAN IF NOT PAST IETIME
C
      IF(IFMT.EQ.'FOF     ')THEN
         IF(IVOL.NE.IVOLOLD)THEN
            CALL PRVOLMFF(IHSK,IPREC,RNGCOR,NFRAME)
            IVOLOLD=IVOL
            ITPOLD=ITP
         END IF
      END IF

      DO 850 I=1,MXF
  850 IGATE(I)=0
      IF(IEOT.EQ.1)THEN
         WRITE(6,851)IUN,NFRAME
  851    FORMAT(4X,' END OF DATA ON UNIT: ',I2,
     +             ' FRAMES PLOTTED=',I6)
         GO TO 5
      END IF
      IF((ITIME+NEWDAY*240000).GE.IETIME)THEN
         WRITE(6,1773)IDATE,ITIME,NEWDAY,IETIME
 1773    FORMAT(1X,' IDATE,ITIME,NEWDAY,IETIME=',I8,2X,I6.6,I8,2X,I6.6)
         WRITE(6,853)NFRAME
  853    FORMAT(4X,'     END PROCESS: FRAMES PLOTTED =',I6)
         GO TO 5
      ELSE
         GO TO 810
      END IF

  900 CONTINUE

      IF(NA.GT.0)CALL PLOTAREA(IDATE)
C     Close GKS workstation and GFLASH
C
      CALL GCLWK(IDWKS)
      CALL CLSGKS
      IF(NFRAME.NE.0)WRITE(6,901)NFRAME
  901 FORMAT(/,4X,'      END OF JOB: FRAMES PLOTTED =',I6,/)

      STOP

C     FATAL ERROR MESSAGES
C
  970 WRITE(6,971)
  971 FORMAT(/,1X,'*** PROCESS COMMAND MUST PRECEED DISPOSE ',
     +            'COMMAND ***',/)
      STOP

  980 WRITE(6,981)
  981 FORMAT(/,1X,'*** PROCESS COMMAND MUST PRECEED PLTSWTH ',
     +            'COMMAND ***',/)
      STOP

  990 WRITE(6,991)
  991 FORMAT(/,1X,'*** PPIWIN, RHIWIN, SURWIN, COPWIN OR BSCAN MUST ',
     +          'PRECEED PROCESS COMMAND ***',/)
      STOP

      END
