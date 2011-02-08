      PROGRAM GETLIGHT
C
C     Test program for getting and plotting lightning data,
C     both NLDN and LMA.
C     MXL    - Aircraft track input values (15,000)
C     MXK    - Landmarks (12,000)
C     MXNL   - NLD CG lightning strikes (100,000)
C     MXLM   - LMA lightning channel (1,000,000)
C     MXSND  - MGLASS sounding levels (1,000)
C

      PARAMETER (MXL=15000,MXK=12000)
      DIMENSION XACT(MXL),YACT(MXL),ZACT(MXL),TACT(MXL)
      DIMENSION UACT(MXL),VACT(MXL),WACT(MXL)
      DIMENSION CACT(MXL),QACT(MXL),DACT(MXL),HACT(MXL)

      DIMENSION XMRK(MXK),YMRK(MXK),ZMRK(MXK),AMRK(MXK)
      DIMENSION NNET(20)
      CHARACTER*7 NMRK(MXK)
      CHARACTER*6 SMRK(20)
C
C     Common blocks for NLDN and LMA positional information
C
      PARAMETER (MXNL=100000,MXLM=1000000)
      COMMON/NLDN/XNLD(MXNL),YNLD(MXNL),TNLD(MXNL),PNLD(MXNL),
     X            HNLD(MXNL),AZNLD(MXNL),INLD,DTNLD
      COMMON/LMA/XLMA(MXLM),YLMA(MXLM),ZLMA(MXLM),TLMA(MXLM),
     X           HLMA(MXLM),AZLMA(MXLM),ILMA,DTLMA
      CHARACTER*4 LMA_COLR

      INCLUDE 'snding.inc'
      INCLUDE 'colors.inc'
      CHARACTER*1 BGFLAG
      CHARACTER*4 GRAYTYP
      DATA IGRPLT,IOLDGY,BGFLAG/0,0,' '/
      DATA GRAYTYP,GSTR,GRAYEST/'LIN ',0.95,0.0/
      DATA IBLACK,IWHITE,IGRAY,IRED,IGREEN,IBLUE/63,64,65,66,67,68/
      DATA ICYAN,IMAGENTA,IYELLOW/69,70,71/
      DATA IBMAGENT,ISBLUE,IORANGE,IFGREEN/72,73,74,75/

      CHARACTER*8 INDAT(10)
      LOGICAL WINSET
      DIMENSION XMIN(8),XMAX(8),YMIN(8),YMAX(8),GXMIN(8),GXMAX(8),
     +     GYMIN(8),GYMAX(8),ANGTOL(8),FXMN(8),FXMX(8),IARCS(8),
     +     AZMIN(8),AZMAX(8),AZROT(8),ITPFLG(8)
      CHARACTER*8 IRATYP,ICORD,IORIGIN

      COMMON /HEMISPHERE/ LATSPHERE,LONSPHERE,LAT_SIGN,LON_SIGN
      CHARACTER*8 LATSPHERE,LONSPHERE
      REAL LAT_SIGN,LON_SIGN
      DATA LATSPHERE,LONSPHERE/'NORTH','WEST'/
      DATA LAT_SIGN,LON_SIGN/+1.0,+1.0/

      CHARACTER*32 MRKFILE,LMAFILE,NLDFILE
      CHARACTER*24 SNDFILE

C     Fractional corners and widths for plotting:
C        XRH,YTH,HSIDE - Horizontal XY-plots (upper right and width)
C        XRT,YTP,XSIDE,YSIDE  - Time-height (upper right and widths)
C        XSL,XSR,YSB,YST - Height-histogram corners
C
      DATA XRH,YTH,HSIDE/0.9,0.9,0.8/
      DATA XRT,YTP,XSIDE,YSIDE/0.65,0.9,0.55,0.4/
      DATA XSL,XSR,YSB,YST/0.7,0.95,0.5,0.9/
      REAL MLAT,MLON

C  Open GKS (unit=2) and (LGFLAS=9) flash buffers
C     Flash buffer #1 - used for SkewT background
C                  #2 - used for network plot
C                  #3 - used for NLDN TZ-plot
C                  #4 - used for NLDN XY-plot
C
      CALL OPNGKS

C     THE FOLLOWING LINE IS FOR GFLAS ROUTINES
C
      LGFLAS=9
      CALL GOPWK(2,LGFLAS,3)

      CALL GSCLIP(0)
      CALL GSPLCI (1)
      CALL GSTXCI(1)

      NFRAME=0
      NC=0

C  Change function code delimiter for ':' to '&'
C  so that we can print a colon on the plots.
C
      CALL PCSETC ('FC', '&')

C  SET THE COLOR TABLE TO DEFAULT RAINBOW, WITH BLACK BACKGROUND
C     INITIAL VALUES: IGRPLT=0, BGFLAG=' '
C     
C     Set the background to white and text to black
C
      IGRPLT=1
      BGLFAG='W'
      CALL SETUSV('LW',1200)
      CALL SETCOL(IGRPLT,BGFLAG,GRAYTYP,GSTR,GRAYEST)

c      ORLAT = 39.36694
c      ORLON = -101.70027
c      ANGXAX=90.0

      OPEN(UNIT=5,FILE='./getlight.inp',STATUS='OLD')
    5 READ(5, 7)(INDAT(I),I=1,10)
    7 FORMAT(10A8)
      WRITE(6, 7)(INDAT(I),I=1,10)
      IF(INDAT(1)(1:1).EQ.'*')THEN
         GO TO 5
      ELSE IF(INDAT(1).EQ.'SURWIN  ')THEN
         GO TO 50
      ELSE IF(INDAT(1).EQ.'GETACT  ')THEN
         GO TO 230
      ELSE IF(INDAT(1).EQ.'GETMRK  ')THEN
         GO TO 240
      ELSE IF(INDAT(1).EQ.'PLTMRK  ')THEN
         GO TO 245
      ELSE IF(INDAT(1).EQ.'LATLON  ')THEN
         GO TO 450
      ELSE IF(INDAT(1).EQ.'GETNLD  ')THEN
         GO TO 500
      ELSE IF(INDAT(1).EQ.'GETLMA  ')THEN
         GO TO 510
      ELSE IF(INDAT(1).EQ.'GETSND  ')THEN
         GO TO 520
      ELSE IF(INDAT(1).EQ.'TS_DRV  ')THEN
         GO TO 530
      ELSE IF(INDAT(1).EQ.'STOP')THEN

C     Close GKS workstation
C
         CALL GCLWK(2)
         CALL CLSGKS
         IF(NFRAME.NE.0)WRITE(6,901)NFRAME
 901     FORMAT(/,8X,'      END OF JOB: FRAMES PLOTTED =',I6,/)
         STOP
      ELSE
         GO TO 5
      END IF

C     INPUT (X,Y) PLOT AND ELEVATION WINDOWS FOR SUR SCAN MODE
C
 50   CALL SURWIN(INDAT,IRATYP,ICORD,XMIN,XMAX,YMIN,YMAX,GXMIN,GXMAX,
     X            GYMIN,GYMAX,ANGTOL,FXMN,FXMX,ICVRT,X0,Y0,AZCOR,IARCS,
     X            IAZC,AZMIN,AZMAX,AZROT,ITPFLG)
      IF(ITPFLG(8).EQ.1)THEN
         WINSET=.TRUE.
      ELSE
         WINSET=.FALSE.
      END IF
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
      MRKFILE=INDAT(2)//INDAT(3)//INDAT(4)//INDAT(5)
      print *,'GETMRK: mrkfile=',mrkfile
c      print *,'GETMRK: imrk,net,nnet=',imrk,net,(nnet(i),i=1,net)
      GO TO 5

C     PLOT LANDMARK (LAT,LON) POSITIONS AND NAMES FOR LATER OVERLAYING
C
 245  READ(INDAT,247)XMN,XMX,YMN,YMX
 247  FORMAT(//////F8.0/F8.0/F8.0/F8.0)

C     Store network plot in flash buffer #2
C
      CALL GFLAS1(2)
      CALL GSCLIP(1)
      CALL PLT_MRK(NMRK,XMRK,YMRK,SMRK,IMRK,MXK,XMN,XMX,YMN,YMX,
     X     XRH,YTH,HSIDE,MRKFILE)
C     PLOT POLITICAL MAP WITHIN FRACTIONAL COORDINATES (FL,FR,FB,FT), THEN
C     CALL SET TO MAP USER COORDINATES (KM) BACK INTO FRACTIONAL ONES.
C     JMAP = (0) NO MAP, (1) SOLID LINES, (2) DOTTED LINES
C     In MAPSTC: 'CO' - continental outlines only, 'US' - US state outlines
C                'PS' - continental, US state, and international outlines,
C                'PO' - continental and international outlines
C     
      print *,'JMAP: olat,olon,angxax=',olat,olon,angxax
      JMAP=1
      IF(JMAP.GE.1)THEN
         CALL XY2LLDRV(BLLAT,BLLON,XMN,YMN,OLAT,OLON,ANGXAX)
         print *,'JMAP: bllat,bllon=',bllat,bllon
         CALL XY2LLDRV(TRLAT,TRLON,XMX,YMX,OLAT,OLON,ANGXAX)
         print *,'JMAP: trlat,trlon=',trlat,trlon
         CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
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
c         CALL MAPROJ('CE',MLAT,MLON,(90.0-ANGXAX))
         CALL MAPROJ('ME',MLAT,MLON,(90.0-ANGXAX))
         CALL MAPSET('CO',BLLAT,BLLON,TRLAT,TRLON)
         CALL MAPPOS(FL,FR,FB,FT)
         CALL MAPINT
         CALL MAPLOT
         CALL SET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
         CALL GSCLIP(0)
      END IF
      CALL GFLAS2(2)

C     Plot the network with GFLAS3(2) call
C
      CALL GFLAS3(2)
      CALL FRAME

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

C     GET NLD CG Lightning (LAT,LON) POSITIONS FOR LATER OVERLAYING
C
 500  CALL GETNLD(INDAT,TNLD,XNLD,YNLD,HNLD,AZNLD,PNLD,MXNL,
     X     INLD,OLAT,OLON,ANGXAX,DTNLD,ORLAT,ORLON,DZNLD)
      NLDFILE=INDAT(2)//INDAT(3)//INDAT(4)//INDAT(5)
      print *,'GETNLD: nldfile=',nldfile
      GO TO 5

C     GET LMA Lightning Channel (LAT,LON) POSITIONS FOR LATER OVERLAYING
C
 510  CALL GETLMA(INDAT,TLMA,XLMA,YLMA,ZLMA,HLMA,AZLMA,MXLM,ILMA,
     X     LMA_COLR,OLAT,OLON,ANGXAX,DTLMA,ORLAT,ORLON,DZLMA)
      LMAFILE=INDAT(2)//INDAT(3)//INDAT(4)//INDAT(5)
      print *,'GETLMA: lmafile=',lmafile
      GO TO 5

C     GET SOUNDING FOR MAPPING TO RADAR SPACE
C     Note: Flash buffer # 1 is used for SkewT background
C
 520  CALL GETSND(INDAT,OLAT,OLON,ANGXAX,ORLAT,ORLON,NFRAME)
      SNDFILE=INDAT(2)//INDAT(3)//INDAT(4)
      print *,'GETSND: sndfile=',sndfile

C     Plot four-panel sounding: 
C          (1) temperature, (2) cloud liquid water content, 
C          (3) mixing ratio, and (4) maximum updraft.
C          Z1_CED,ZD_CED,NZ_CED - are Cedric min, del, and nmb
C
      CALL PLTSND(SNDFILE)
      GO TO 5

C     Call plot driver for NLD and LMA lightning data.
C
 530  CALL TS_DRV(INDAT,OLAT,OLON,ANGXAX,ORLAT,ORLON,
     X     TNLD,XNLD,YNLD,HNLD,AZNLD,PNLD,MXNL,INLD,DTNLD,DZNLD,
     X     TLMA,XLMA,YLMA,ZLMA,HLMA,AZLMA,MXLM,ILMA,DTLMA,DZLMA,
     X     LMA_COLR,XRT,YTP,XSIDE,YSIDE,XSL,XSR,YSB,YST,XRH,YTH,
     X     HSIDE,LMAFILE,NLDFILE,MRKFILE,SNDFILE)
      GO TO 5

      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE TS_DRV(INDAT,OLAT,OLON,ANGXAX,ORLAT,ORLON,
     X     TNLD,XNLD,YNLD,HNLD,AZNLD,PNLD,MXNL,INLD,DTNLD,DZNLD,
     X     TLMA,XLMA,YLMA,ZLMA,HLMA,AZLMA,MXLM,ILMA,DTLMA,DZLMA,
     X     LMA_COLR,XRT,YTP,XSIDE,YSIDE,XSL,XSR,YSB,YST,XRH,YTH,
     X     HSIDE,LMAFILE,NLDFILE,MRKFILE,SNDFILE)
C
C     Driver for plotting LMA and NLDN time-series and horizontal plots
C     Input T_MN, T_MX, and TINC.  Driver increments through times.
C
C     Fractional corners and widths for plotting:
C        XRH,YTH,HSIDE - Horizontal XY-plots (upper right and width)
C        XRT,YTP,XSIDE,YSIDE  - Time-height (upper right and widths)
C        XSL,XSR,YSB,YST - Height-histogram corners
C
C
      INCLUDE 'snding.inc'
      INCLUDE 'colors.inc'
      CHARACTER TLAB*18
      CHARACTER IXNAM*14,IYNAM*11
      CHARACTER IFMTX*6,IFMTY*6

      CHARACTER*32 MRKFILE,LMAFILE,NLDFILE
      CHARACTER*24 SNDFILE
      CHARACTER*8 INDAT(10)
      CHARACTER*8 BLANK

      DATA CSIZ/15.0/
      DATA BLANK/'        '/
      DATA TORAD,TODEG/0.017453293,57.29577951/

      DIMENSION XNLD(MXNL),YNLD(MXNL),TNLD(MXNL),PNLD(MXNL)
      DIMENSION HNLD(MXNL),AZNLD(MXNL)

      DIMENSION XLMA(MXLM),YLMA(MXLM),ZLMA(MXLM)
      DIMENSION TLMA(MXLM)
      DIMENSION HLMA(MXLM),AZLMA(MXLM)
      CHARACTER*4 LMA_COLR

      DIMENSION UPMAX(40)

C     Read plotting parameters for LMA time-series and horizontal plots
C
      print *,'INDAT:',indat
      READ(INDAT,37)T_MN,T_MX,TINC,HMN,HMX,XMN,XMX,YMN,YMX
 37   FORMAT(/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0)
      print *,'DRV: xrt,ytp,xside,yside=',xrt,ytp,xside,yside
      print *,'DRV:     xsl,xsr,ysb,yst=',xsl,xsr,ysb,yst

      IF(T_MN.EQ.0.0 .AND. T_MX.EQ.0.0)THEN
         PRINT *,'***ERROR IN TIME WINDOW: BOTH MIN and MAX ARE 0***'
         STOP
      END IF
C     Convert input HHMMSS to seconds
C
      IHR=INT(T_MN/10000.0)
      IMN=(T_MN-10000.0*INT(T_MN/10000.0))/100.0
      ISC=T_MN-IHR*10000.0-IMN*100.0
      TS_MN=IHR*3600.0+IMN*60.0+ISC
      print *,'T_MN,IHR,IMN,ISC,TS_MN=',T_MN,IHR,IMN,ISC,TS_MN

      IHR=INT(T_MX/10000.0)
      IMN=(T_MX-10000.0*INT(T_MX/10000.0))/100.0
      ISC=T_MX-IHR*10000.0-IMN*100.0
      TS_MX=IHR*3600.0+IMN*60.0+ISC
      print *,'T_MX,IHR,IMN,ISC,TS_MX=',T_MX,IHR,IMN,ISC,TS_MX

      IHR=INT(TINC/10000.0)
      IMN=(TINC-10000.0*INT(TINC/10000.0))/100.0
      ISC=TINC-IHR*10000.0-IMN*100.0
      TS_IN=IHR*3600.0+IMN*60.0+ISC
      print *,'TINC,IHR,IMN,ISC,TS_MX=',TINC,IHR,IMN,ISC,TS_IN

      NTIMES = 1.1 + (TS_MX-TS_MN)/TS_IN

      DO N=1,NTIMES
         TMN_SEC=TS_MN+(N-1)*TS_IN
         TMX_SEC=TMN_SEC+TS_IN

C     Convert seconds (TMN_SEC,TMX_SEC) back to HHMMSS (TMN,TMX)
C
         IHR = TMN_SEC/3600.0
         IMN = (TMN_SEC - IHR*3600.0)/60.0
         ISC = (TMN_SEC - IHR*3600.0 - IMN*60.0)
         TMN = IHR*10000+IMN*100+ISC

         IHR = TMX_SEC/3600.0
         IMN = (TMX_SEC - IHR*3600.0)/60.0
         ISC = (TMX_SEC - IHR*3600.0 - IMN*60.0)
         TMX = IHR*10000+IMN*100+ISC
         print *,'DRV: N,TMN,TMX=',n,tmn,tmx
         
C     PLOT NLD CG Lightning (LAT,LON) POSITIONS FOR LATER OVERLAYING
C     
         CALL TS_NLD(TNLD,XNLD,YNLD,HNLD,AZNLD,PNLD,MXNL,
     X        INLD,OLAT,OLON,ANGXAX,DTNLD,ORLAT,ORLON,DZNLD,
     X        XRT,YTP,XSIDE,YSIDE,XSL,XSR,YSB,YST,XRH,YTH,HSIDE,
     X        LMAFILE,NLDFILE,MRKFILE,SNDFILE,TMN,TMX,HMN,HMX,
     X        XMN,XMX,YMN,YMX)
         
C     PLOT LMA Lightning Channel (LAT,LON) POSITIONS
C     
         CALL TS_LMA(TLMA,XLMA,YLMA,ZLMA,HLMA,AZLMA,MXLM,ILMA,
     X        LMA_COLR,OLAT,OLON,ANGXAX,DTLMA,ORLAT,ORLON,DZLMA,
     X        XRT,YTP,XSIDE,YSIDE,XSL,XSR,YSB,YST,XRH,YTH,HSIDE,
     X        LMAFILE,NLDFILE,MRKFILE,SNDFILE,TMN,TMX,HMN,HMX,
     X        XMN,XMX,YMN,YMX)
      END DO

      RETURN
      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE TS_NLD(TNLD,XNLD,YNLD,HNLD,AZNLD,PNLD,MXNL,
     X     INLD,OLAT,OLON,ANGXAX,DTNLD,ORLAT,ORLON,DZNLD,
     X     XRT,YTP,XSIDE,YSIDE,XSL,XSR,YSB,YST,XRH,YTH,HSIDE,
     X     LMAFILE,NLDFILE,MRKFILE,SNDFILE,TMN,TMX,HMN,HMX,
     X     XMN,XMX,YMN,YMX)
C
C  READ IN NLDN CG STRIKE POSITIONS (TIME-LAT-LON-POLARITY) FROM NLDFILE:
C     This routine assumes that two successive days have been combined 
C     into a single file to handle crossing midnight (UTC).  Twenty-four
C     will be added to the hour of the second day.
C
C     CONVERT THEM TO (X,Y) FROM (OLAT,OLON)
C  NOTE: (OLAT,OLON) MUST BE THE (LAT,LON) OF THE PLOTTING WINDOW ORIGIN,
C        WHERE THE WINDOW IS DEFINED BY (GXMIN,GXMAX,GYMIN,GYMAX)
C
C  EXAMPLE FOR NATIONAL:
C
C  // File D00138.nldn opened on Wed May 17 16:54:16 2000
C  05/17/:0 16:53:40 39.300  -86.038   -80.0  1
C
C     TNLD      - Time of the strike (sec)
C     XNLD,YNLD - (X,Y) position (km) of CG strike relative to origin
C     HNLD      - Horizontal range (km) of CG strike
C     AZNLD     - Azimuth angle (deg) of CG strike
C     PNLD      - Polarity of ground strike and value (Kv/m)
C     DTNLD     - Time interval for plotting strikes.  Plot points within
C                 a time window relative to the current radar scan.
C                 DTNLD .gt. 0: Plot +/- DTNLD from central time (RMSEC)
C                 DTNLD .le. 0: Plot within DTNLD seconds outside the
C                               radar scan time interval
C     DZNLD     - Height/Azimuth interval for plotting CG strike points
C
C     Fractional corners and widths for plotting:
C        XRH,YTH,HSIDE - Horizontal XY-plots (upper right and width)
C        XRT,YTP,XSIDE,YSIDE  - Time-height (upper right and widths)
C        XSL,XSR,YSB,YST - Height-histogram corners
C
      DIMENSION XNLD(MXNL),YNLD(MXNL),TNLD(MXNL),PNLD(MXNL)
      DIMENSION HNLD(MXNL),AZNLD(MXNL)
      CHARACTER*32 MRKFILE,LMAFILE,NLDFILE
      CHARACTER*24 SNDFILE
      CHARACTER IDIR*4
      CHARACTER*41 HEADER
      CHARACTER*8 BLANK
      LOGICAL FIRST
      INTEGER POS_CG,POS_PERCENT,NEG_CG,NEG_PERCENT

c      DATA INLD_MOD/100/
      DATA INLD_MOD/1/
      DATA BLANK/'        '/
      DATA TORAD,TODEG/0.017453293,57.29577951/

      INCLUDE 'snding.inc'
      INCLUDE 'colors.inc'
      CHARACTER TLAB*18
      CHARACTER IXNAM*14,IYNAM*11
      CHARACTER IFMTX*6,IFMTY*6
      DATA CSIZ/15.0/

      print *,'NLD: xrt,ytp,xside,yside=',xrt,ytp,xside,yside
      print *,'NLD:     xsl,xsr,ysb,yst=',xsl,xsr,ysb,yst

C     Convert TMN=HHMMSS and TMX=HHMMSS to seconds
C
      IHR=INT(TMN/10000.0)
      IMN=(TMN-10000.0*INT(TMN/10000.0))/100.0
      ISC=TMN-IHR*10000.0-IMN*100.0
      TS_MN=IHR*3600.0+IMN*60.0+ISC
      print *,'NLD: TMN,IHR,IMN,ISC,TS_MN=',TMN,IHR,IMN,ISC,TS_MN

      IHR=INT(TMX/10000.0)
      IMN=(TMX-10000.0*INT(TMX/10000.0))/100.0
      ISC=TMX-IHR*10000.0-IMN*100.0
      TS_MX=IHR*3600.0+IMN*60.0+ISC
      print *,'NLD: TMX,IHR,IMN,ISC,TS_MX=',TMX,IHR,IMN,ISC,TS_MX
      print *,'NLD: HMN,HMX,XMN,XMX,YMN,YMX=',HMN,HMX,XMN,XMX,YMN,YMX
      WRITE(TLAB,41)INT(TMN),INT(TMX)
 41   FORMAT(I6.6,'--',I6.6,' UTC')

C     Plot all NLD data points within the time-space window
C

C     Flash buffer #3 - used for NLDN TZ-plot
C                  #4 - used for NLDN XY-plot
C
C     TZ-plot of NLDN data --> Flash buffer #3
C
      CALL GFLAS1(3)
c      CALL REF_GRID
      CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
      X2=XRT
      X1=XRT-XSIDE
      Y1=YTP-YSIDE
      Y2=YTP
      CALL SET(0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
      XP=X1
      YP=Y2+0.025
      CALL PLCHMQ(XP,YP,NLDFILE,12.0,0.0,-1.0)
      XP=X2
      YP=Y2+0.025
      CALL PLCHMQ(XP,YP,TLAB,12.0,0.0,1.0)
      IXNAM = 'Time (seconds)'
      XP=0.5*(X1+X2)
      YP=Y1-0.05
      CALL PLCHMQ(XP,YP,IXNAM,12.0,0.0,0.0)
      IYNAM = 'Height (km)'
      XP=X1-0.05
      YP=0.5*(Y1+Y2)
      CALL PLCHMQ(XP,YP,IYNAM,12.0,90.0,0.0)
      CALL MAJMIN(TS_MN,TS_MX,IFMTX,MJRX,MNRX,IPLX)
      CALL MAJMIN(HMN,HMX,IFMTY,MJRY,MNRY,IPLY)
      CALL SET(X1,X2,Y1,Y2,TS_MN,TS_MX,HMN,HMX,1)
      CALL LABMOD(IFMTX,IFMTY,IPLX,IPLY,12,12,8,8,0)
      CALL GRIDAL(MJRX,MNRX,MJRY,MNRY,1,1,5,X1,Y1)

      CALL GSPLCI(IRED)
      CALL GSTXCI(IRED)
      ZP=HMN+0.5
      JNLD=0
      DO I=1,INLD
         TP=TNLD(I)
         XP=XNLD(I)
         YP=YNLD(I)
         POLAR=PNLD(I)
         IF((TP .GE. TS_MN .AND. TP .LE. TS_MX) .AND.
     X      (XP .GE. XMN   .AND. XP .LE. XMX  ) .AND.
     X      (YP .GE. YMN   .AND. YP .LE. YMX  ))THEN
            print *,'TZ-NLD: T,X,Y=',tp,xp,yp
            IF(POLAR.GT.0.0)THEN
               CALL PLCHMQ (TP,ZP,'X',CSIZ,0.0,0.0)
            ELSE
               CALL PLCHMQ (TP,ZP,'0',CSIZ,0.0,0.0)
            END IF
            JNLD=JNLD+1
         END IF
      END DO
      print *,'TZ-JNLD: JNLD=',jnld

      CALL SFLUSH
      CALL GSPLCI(1)
      CALL GSTXCI(1)

      ZREF=BASEZ

C     Draw ZREF reference thick, red dashed line
C
      CALL SFLUSH
      CALL GETUSV('LW',ILW)
      JLW=1800
      CALL SETUSV('LW',JLW)
      CALL GSPLCI (IRED)
      CALL DASHDB (O'170360')
      CALL LINED(TS_MN,ZREF,TS_MX,ZREF)
      CALL GFLAS2(3)

C     Restore line thickness and color
C
      CALL SFLUSH
      CALL SETUSV('LW',ILW)
      CALL GSPLCI (1)

C     Create XY-plots of NLDN data --> Flash buffer #4
C     Scale the fractional plot widths for DX .NE. DY
C
      DX=XMX-XMN
      DY=YMX-YMN
      RAT_XY=DX/DY
      RAT_YX=DY/DX
      IF(RAT_XY.GT.RAT_YX)THEN
         XHSIDE=HSIDE
         YHSIDE=HSIDE*RAT_YX
      ELSE
         XHSIDE=HSIDE*RAT_XY
         YHSIDE=HSIDE
      END IF
      XL=XRH-XHSIDE
      XR=XRH
      YB=YTH-YHSIDE
      YT=YTH
      print *,'PLT_NLD:   xmn,xmx,dx=',xmn,xmx,dx
      print *,'PLT_NLD:   ymn,ymx,dy=',ymn,ymx,dy
      print *,'PLT_NLD: xl,xr,xhside=',xl,xr,xhside
      print *,'PLT_NLD: yb,yt,yhside=',yb,yt,yhside

      CALL GFLAS1(4)
      CALL PLT_NLD(XNLD,YNLD,PNLD,TNLD,MXNL,INLD,XMN,XMX,
     X     YMN,YMX,TS_MN,TS_MX,XL,XR,YB,YT,NLDFILE,TLAB)
      CALL GFLAS2(4)
      RETURN
      END
c     
c----------------------------------------------------------------------X
c
      SUBROUTINE TS_LMA(TLMA,XLMA,YLMA,ZLMA,HLMA,AZLMA,MXLM,
     X     ILMA,LMA_COLR,OLAT,OLON,ANGXAX,DTLMA,ORLAT,ORLON,DZLMA,
     X     XRT,YTP,XSIDE,YSIDE,XSL,XSR,YSB,YST,XRH,YTH,HSIDE,
     X     LMAFILE,NLDFILE,MRKFILE,SNDFILE,TMN,TMX,HMN,HMX,
     X     XMN,XMX,YMN,YMX)
C
C  READ IN LMA LIGHTNING CHANNEL (TIME-LAT-LON-HEIGHT) FROM LMAFILE:
C     CONVERT THEM TO (X,Y,Z) FROM (OLAT,OLON)
C  NOTE: (OLAT,OLON) MUST BE THE (LAT,LON) OF THE PLOTTING WINDOW ORIGIN,
C        WHERE THE WINDOW IS DEFINED BY (GXMIN,GXMAX,GYMIN,GYMAX)
C
C  EXAMPLE FOR NMT:
C
C New Mexico Tech's Lightning Mapping System -- Ananlyzed Data
C Data start time: 07/12/00 23:00:00
C Number of seconds analyzed:  600
C Location: STEPS
C Analysis program: /usr/local/nfs/bin/analysis_v5
C Analysis started : Mon Jan 29 19:51:11 2001
C Analysis finished: Mon Jan 29 19:55:03 2001
C Number of active stations: 11
C Active stations: B C D E F H I J K L N 
C Data: time (UT sec of day), lat, lon, alt(m), reduced chi^2, # of stations contributed
C Data format: 15.9f 10.6f 10.6f 7.1f 5.2f 2d
C Number of events:        10202
C *** data ***
C  82800.002450609  40.175552 -103.544895 10887.1  0.94 11
C  82800.002755029  40.175492 -103.541112 10302.5  0.14  8
C
C     TLMA      - Time of the strike (sec)
C     XLMA,YLMA - (X,Y) position of landmark relative to origin
C     ZLMA      - Height of LMA lightning channel
C     HLMA      - Horizontal range (km) of lightning channel
C     AZLMA     - Azimuth angle (deg) of lightning channel
C     LMA_COLR  - WHI (white), BLK (black), GRY (gray), RED (red),
C                 GRN (green), BLU (blue), CYA (cyan), MAG (magenta),
C                 YEL (yellow), BMG (blue-magenta), SBL (sky-blue),
C                 ORN (orange), FGR (forest-green)
C     DTLMA     - Time interval for plotting strikes.  Plot points within
C                 a time window relative to the current radar scan.
C                 DTLMA .gt. 0: Plot +/- DTLMA from central time (RMSEC)
C                 DTLMA .le. 0: Plot within DTLMA seconds outside the
C                               radar scan time interval
C     DZLMA     - Height interval for plotting strikes.  Plot points within
C                 a height window relative to the current radar scan.
C                 DZLMA .gt. 0: Plot +/- DZLMA from radar scan height
C                 DZLMA .le. 0: Plot within any height interval
C
C     Fractional corners and widths for plotting:
C        XRH,YTH,HSIDE - Horizontal XY-plots (upper right and width)
C        XRT,YTP,XSIDE,YSIDE  - Time-height (upper right and widths)
C        XSL,XSR,YSB,YST - Height-histogram corners
C
      DIMENSION XLMA(MXLM),YLMA(MXLM),ZLMA(MXLM)
      DIMENSION TLMA(MXLM)
      DIMENSION HLMA(MXLM),AZLMA(MXLM)
      CHARACTER*32 MRKFILE,LMAFILE,NLDFILE
      CHARACTER*24 SNDFILE,PERCENT
      CHARACTER IDIR*4
      CHARACTER*60 HEADER
      CHARACTER*8 BLANK
      CHARACTER*4 LMA_COLR
      DOUBLE PRECISION SEC,FSEC,RLAT,RLON

      DATA ILMA_MOD/1000/
c      DATA ILMA_MOD/1/
      DATA BLANK/'        '/
      DATA TORAD,TODEG/0.017453293,57.29577951/

      INCLUDE 'snding.inc'
      INCLUDE 'colors.inc'
      CHARACTER*18 TLAB
      CHARACTER IXNAM*14,IYNAM*11
      CHARACTER IFMTX*6,IFMTY*6
      DATA CSIZ/15.0/

      PARAMETER (MXBIN=200)
      DIMENSION ZBIN(MXBIN),HBIN(MXBIN)

      print *,'LMA: xrt,ytp,xside,yside=',xrt,ytp,xside,yside
      print *,'LMA:     xsl,xsr,ysb,yst=',xsl,xsr,ysb,yst

C     Clear vertical histogram of LMA events
C
      HBMN=HMN
      HBMX=HMX
      HINC=0.2
      NTOT=(HBMX-HBMN)/HINC
      print *,'LMA bins: n,hbmn,hbmx,hinc=',ntot,hbmn,hbmx,hinc
      DO I=1,NTOT
         HBIN(I)=0.0
         ZBIN(I)=0.5*HINC+(I-1)*HINC
c         print *,'LMA bins: i,zbin=',i,zbin(i)
      END DO

C     Convert TMN=HHMMSS and TMX=HHMMSS to seconds
C
      IHR=INT(TMN/10000.0)
      IMN=(TMN-10000.0*INT(TMN/10000.0))/100.0
      ISC=TMN-IHR*10000.0-IMN*100.0
      TS_MN=IHR*3600.0+IMN*60.0+ISC
      print *,'LMA: TMN,IHR,IMN,ISC,TS_MN=',TMN,IHR,IMN,ISC,TS_MN

      IHR=INT(TMX/10000.0)
      IMN=(TMX-10000.0*INT(TMX/10000.0))/100.0
      ISC=TMX-IHR*10000.0-IMN*100.0
      TS_MX=IHR*3600.0+IMN*60.0+ISC
      print *,'LMA: TMX,IHR,IMN,ISC,TS_MX=',TMX,IHR,IMN,ISC,TS_MX
      print *,'LMA: HMN,HMX,XMN,XMX,YMN,YMX=',HMN,HMX,XMN,XMX,YMN,YMX
      WRITE(TLAB,41)INT(TMN),INT(TMX)
 41   FORMAT(I6.6,'--',I6.6,' UTC')

C     Plot all LMA data points within the time-space window
C

C     Flash buffer #3 - used for NLDN TZ-plot
C                  #4 - used for NLDN XY-plot
C
C     TZ-plot of LMA data
C
c      CALL REF_GRID
      CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
      X2=XRT
      X1=XRT-XSIDE
      Y1=YTP-YSIDE
      Y2=YTP
      CALL SET(0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
      XP=X1
      YP=Y2+0.05
      CALL PLCHMQ(XP,YP,LMAFILE,12.0,0.0,-1.0)
      XP=X2
      YP=Y2+0.025
      CALL PLCHMQ(XP,YP,TLAB,12.0,0.0,1.0)
      IXNAM = 'Time (seconds)'
      XP=0.5*(X1+X2)
      YP=Y1-0.05
      CALL PLCHMQ(XP,YP,IXNAM,12.0,0.0,0.0)
      IYNAM = 'Height (km)'
      XP=X1-0.05
      YP=0.5*(Y1+Y2)
      CALL PLCHMQ(XP,YP,IYNAM,12.0,90.0,0.0)
      CALL MAJMIN(TS_MN,TS_MX,IFMTX,MJRX,MNRX,IPLX)
      CALL MAJMIN(HMN,HMX,IFMTY,MJRY,MNRY,IPLY)
      CALL SET(X1,X2,Y1,Y2,TS_MN,TS_MX,HMN,HMX,1)
      CALL LABMOD(IFMTX,IFMTY,IPLX,IPLY,12,12,8,8,0)
      CALL GRIDAL(MJRX,MNRX,MJRY,MNRY,1,1,5,X1,Y1)

      JLMA=0
      DO I=1,ILMA
         XP=XLMA(I)
         YP=YLMA(I)
         ZP=ZLMA(I)
         TP=TLMA(I)
         IF((TP .GE. TS_MN .AND. TP .LE. TS_MX) .AND.
     X      (XP .GE. XMN   .AND. XP .LE. XMX  ) .AND.
     X      (YP .GE. YMN   .AND. YP .LE. YMX  ) .AND.
     X      (ZP .GE. HMN   .AND. ZP .LE. HMX  ))THEN
            print *,'TZ-LMA: T,X,Y,Z=',tp,xp,yp,zp
            CALL PLCHMQ(TP,ZP,'*',6.0,0.0,0.0)
            JLMA=JLMA+1
            NBIN=1 + (ZP-HBMN)/HINC
            IF(NBIN.LE.NTOT)THEN
               HBIN(NBIN)=HBIN(NBIN)+1.0
            END IF
         END IF
      END DO
      print *,'TZ-JLMA: JLMA=',jlma
      
C     Convert vertical histogram of LMA events to percent
C
      TOT_LMA=FLOAT(JLMA)
      IF(TOT_LMA.GE.1.0)THEN
         DO J=1,NTOT
            CBIN=HBIN(J)
            HBIN(J)=100.0*HBIN(J)/TOT_LMA
         END DO
         DO J=1,NTOT
            print *,'LMA bins: j,zbin,c,p=',j,zbin(j),cbin,hbin(j)
         END DO
      END IF

      ZREF=BASEZ

C     Draw ZREF reference thick, red dashed line
C
      CALL SFLUSH
      CALL GETUSV('LW',ILW)
      JLW=1800
      CALL SETUSV('LW',JLW)
      CALL GSPLCI (IRED)
      CALL DASHDB (O'170360')
      CALL LINED(TS_MN,ZREF,TS_MX,ZREF)

C     Restore line thickness and color
C
      CALL SFLUSH
      CALL SETUSV('LW',ILW)
      CALL GSPLCI (1)

C     Plot vertical histogram of LMA events
C     
      XMIN=0.0
      XMAX=6.0
      PERCENT='PerCent LMA Events'
      CALL PLT_HIST(HBIN,ZBIN,MXBIN,NTOT,HINC,XMIN,XMAX,HMN,HMX,
     X     ZREF,XSL,XSR,YSB,YST,PERCENT,JLMA)

C     Add TZ-plot of NLDN data (flash buffer #3) to this frame
C
      CALL GFLAS3(3)
      CALL FRAME

C     Create XY-plots of LMA, NLD, and overlaid network
C     Scale the fractional plot widths for DX .NE. DY
C
      DX=XMX-XMN
      DY=YMX-YMN
      RAT_XY=DX/DY
      RAT_YX=DY/DX
      IF(RAT_XY.GT.RAT_YX)THEN
         XHSIDE=HSIDE
         YHSIDE=HSIDE*RAT_YX
      ELSE
         XHSIDE=HSIDE*RAT_XY
         YHSIDE=HSIDE
      END IF
      XL=XRH-XHSIDE
      XR=XRH
      YB=YTH-YHSIDE
      YT=YTH
      print *,'PLT_LMA:   xmn,xmx,dx=',xmn,xmx,dx
      print *,'PLT_LMA:   ymn,ymx,dy=',ymn,ymx,dy
      print *,'PLT_LMA: xl,xr,xhside=',xl,xr,xhside
      print *,'PLT_LMA: yb,yt,yhside=',yb,yt,yhside

C     Loop over several vertical slabs of thickness DZ
C     and plot LMA.  This will reveal any LMA holes.
C
c      DZ=1.0
c      NL=(HMX-HMN)/DZ
c      DO L=1,NL-1
c         ZMN=HMN+L*DZ
c         ZMX=ZMN+DZ
      ZMN=HMN
      ZMX=HMX
         CALL PLT_LMA(XLMA,YLMA,ZLMA,TLMA,MXLM,ILMA,XMN,XMX,
     X        YMN,YMX,ZMN,ZMX,TS_MN,TS_MX,XL,XR,YB,YT,LMAFILE,
     X        TLAB)

C        Overlay NLD (flash buffer #4) and network (flash buffer #2)
C
         CALL GFLAS3(4)
         CALL GFLAS3(2)
         CALL FRAME
c      END DO

      RETURN
      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE PLTSND(SNDFILE)
C
C     Routine to plot environmental temperature, cloud liquid 
C     water content, mixing ratio, and maximum updraft.
C
      INCLUDE 'snding.inc'
      INCLUDE 'colors.inc'
      CHARACTER*34 TITLE
      CHARACTER*24 SNDFILE
      CHARACTER*15 IXNAM,IYNAM

c      PARAMETER (MXS=500)
c      COMMON /SOUND/ ZSOUND(MXS),TEMARR(MXS),PPARR(MXS),RMXARR(MXS),            
c     1               THE(MXS),USND(MXS),VSND(MXS),WCLD(MXS),NSOUND,
c     2               WL,RMX0,PLCL,TLCL,ZLCL

      WRITE(TITLE,11)SNDFILE
 11   FORMAT('SOUNDING= ',A24)
      NPT=ISND

C     Set the vertical axis (height) bounds
C
      YMIN = 0.0
      YMAX = 15.0
      YR = BASEZ
      
C     Draw a reference grid for positioning titles
C
      CALL SET(0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
c      CALL REF_GRID
      
      CALL PLCHMQ(0.1,0.975,TITLE,15.0,0.,-1.)

C     Plot environmental temperature (deg C)
C     
      NPLT=1
      IXNAM = 'EnvCldTemp'
      IYNAM = 'Height (km)'
      XMIN=-60.0
      XMAX=30.0
      CALL PLT_SND(TSND,ZSND,MXSND,NPT,IXNAM,XMIN,XMAX,
     X     IYNAM,YMIN,YMAX,YR,NPLT)
      CALL PLT_SND(TCLD,ZSND,MXSND,NPT,IXNAM,XMIN,XMAX,
     X     IYNAM,YMIN,YMAX,YR,NPLT)

C     Plot in-cloud temperature (deg C)
C     
      NPLT=2
      IXNAM = 'DifTemp'
      IYNAM = 'Height (km)'
      XMIN=-5.0
      XMAX=10.0
      CALL PLT_SND(DTEMP,ZSND,MXSND,NPT,IXNAM,XMIN,XMAX,
     X     IYNAM,YMIN,YMAX,YR,NPLT)
      
C     Plot cloud liquid water content (g/m3)
C     
      NPLT=3
      IXNAM = 'Clwc'
      IYNAM = 'Height (km)'
      XMIN=0.0
      XMAX=5.0
      CALL PLT_SND(CLDLWC,ZSND,MXSND,NPT,IXNAM,XMIN,XMAX,
     X     IYNAM,YMIN,YMAX,YR,NPLT)

C     Plot mixing ratio (g/kg)
C     
      NPLT=4
      IXNAM = 'MixRat'
      IYNAM = 'Height (km)'
      XMIN=0.0
      XMAX=15.0
      CALL PLT_SND(CLDMIX,ZSND,MXSND,NPT,IXNAM,XMIN,XMAX,
     X     IYNAM,YMIN,YMAX,YR,NPLT)
      
C     Plot maximum updraft (m/s)
C     
c      DO I=1,NZ_CED
c         ZCED(I)=Z1_CED+(I-1)*ZD_CED
cc         print *,'PLTSND: i,z,w=',i,zced(i),upmax(i)
c      END DO
c      NPLT=4
c      IXNAM = 'Updraft max'
c      IYNAM = 'Height (km)'
c      XMIN=0.0
c      XMAX=50.0
c      CALL PLT_SND(UPMAX,ZCED,40,NZ_CED,IXNAM,XMIN,XMAX,
c     X     IYNAM,YMIN,YMAX,YR,NPLT)

      CALL FRAME
      
      RETURN
      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE PLT_SND(X,Y,MXSND,NPT,IXNAM,XMN,XMX,IYNAM,YMN,YMX,
     X     YR,NPLT)
C
C     Plot a scattergram of environmental sounding X vs. Y
C
C     MXSND - Maximum number of sounding heights
C     NPT   - Actual number of sounding heights
C
C     Abscissa (x):
C        X       - Values of variable to be plotted
C        IXNAM   - Name of the variable 
C        XMN,XMX - Minimum and maximum values for plotting
C
C     Ordinate (y):
C        Y       - Values of variable to be plotted
C        IYNAM   - Name of the variable 
C        YMN,YMX - Minimum and maximum values for plotting
C
      CHARACTER*6 IFMTX,IFMTY
      CHARACTER LABF*15

      CHARACTER*15 IXNAM,IYNAM
      DIMENSION X(MXSND),Y(MXSND)
      
      DIMENSION XRT(4),YTP(4)

      DATA XRT/0.50,0.95,0.50,0.95/
      DATA YTP/0.95,0.95,0.50,0.50/
      DATA XSIDE,YSIDE/0.4,0.4/

      X2=XRT(NPLT)
      X1=XRT(NPLT)-XSIDE
      Y2=YTP(NPLT)
      Y1=YTP(NPLT)-YSIDE
      print *,'PLT_SND: mxsnd,npt=',mxsnd,npt
      print *,'PLT_SND: x1,x2,y1,y2=',x1,x2,y1,y2

C     Scatter X vs. Y
C     
      LSIZE = 15
      CALL MAJMIN(XMN,XMX,IFMTX,MJRX,MNRX,IPLX)
      CALL MAJMIN(YMN,YMX,IFMTY,MJRY,MNRY,IPLY)
      CALL SET(X1,X2,Y1,Y2,XMN,XMX,YMN,YMX,1)
      CALL LABMOD(IFMTX,IFMTY,IPLX,IPLY,LSIZE,LSIZE,8,8,0)
      CALL PERIML(MJRX,MNRX,MJRY,MNRY)


      DO 20 N=1,NPT
         XC=X(N)
         YC=Y(N)
         IF(XC.EQ.-999.0 .AND. YC.EQ.-999.0)GO TO 20
         IF(XC.EQ.0.0 .AND. YC.EQ.0.0)GO TO 20
         IF(XC.LT.XMN.OR.XC.GT.XMX)GO TO 20
         IF(YC.LT.YMN.OR.YC.GT.YMX)GO TO 20
c         CALL PLCHMQ(XC,YC,'+',1.0,0.0,0.0)
         IF(N.GT.1)THEN
            XB=X(N-1)
            YB=Y(N-1)
            CALL LINE(XB,YB,XC,YC)
         END IF
 20   CONTINUE

      CALL DASHDB (O'170360')
      CALL LINED(XMN,YR,XMX,YR)

      CALL SFLUSH
      CALL SETUSV('LW',1500)
      
C     Label the current plot and call frame
C
      CALL SFLUSH
      CALL GSPLCI(1)
      CALL GSTXCI(1)
      CALL SETUSV('LW',1000)
      CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)

      CSIZE = 15.0

      IF(NPLT.EQ. 1 .OR. NPLT.EQ.3)THEN
         WRITE(LABF,31)IYNAM
 31      FORMAT(A15)
         XP=X1-0.06
         YP=0.5*(Y1+Y2)
         CALL PLCHMQ (XP, YP, LABF, CSIZE, 90.0, 0.0)
      END IF

      WRITE(LABF,33)IXNAM
 33   FORMAT(A15)
      XP=X1+0.02
      YP=Y2-0.02
      CALL PLCHMQ (XP, YP, LABF, CSIZE, 0.0, -1.0)

      RETURN
      END
c     
c----------------------------------------------------------------------X
c
      SUBROUTINE PLT_MRK(NMRK,XMRK,YMRK,SMRK,IMRK,MXK,XMN,XMX,YMN,YMX,
     X     XRH,YTH,HSIDE,MRKFILE)

C  Plot XY-locations of landmarks data.
C
C     XMRK,YMRK - (X,Y) position (km) of landmark
C     NMRK,SMRK - Name and symbol for landmark
C     XMN,XMX   - X bounds of the plot domain
C     YMN,YMX   - Y bounds of the plot domain
C
C     Fractional corners and widths for plotting:
C        XRH,YTH,HSIDE - Horizontal XY-plots (upper right and width)
C
      CHARACTER*6 IFMTX,IFMTY
      CHARACTER IXNAM*28,IYNAM*28
      CHARACTER*32 MRKFILE

      DIMENSION XMRK(MXK),YMRK(MXK)
      CHARACTER*7 NMRK(MXK)
      CHARACTER*6 SMRK(20)
      CHARACTER LAB6*6,LAB9*9

C     Create XY-plot of network --> Flash buffer #2
C     Scale the fractional plot widths for DX .NE. DY
C
      DX=XMX-XMN
      DY=YMX-YMN
      RAT_XY=DX/DY
      RAT_YX=DY/DX
      IF(RAT_XY.GT.RAT_YX)THEN
         XHSIDE=HSIDE
         YHSIDE=HSIDE*RAT_YX
      ELSE
         XHSIDE=HSIDE*RAT_XY
         YHSIDE=HSIDE
      END IF
      XL=XRH-XHSIDE
      XR=XRH
      YB=YTH-YHSIDE
      YT=YTH
      print *,'PLTMRK:   xmn,xmx,dx=',xmn,xmx,dx
      print *,'PLTMRK:   ymn,ymx,dy=',ymn,ymx,dy
      print *,'PLTMRK: xl,xr,xhside=',xl,xr,xhside
      print *,'PLTMRK: yb,yt,yhside=',yb,yt,yhside

      X1=XL
      X2=XR
      Y1=YB
      Y2=YT

C     Add labels for title and axes
C
      CALL SET(0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
      XP=X2
      YP=Y2+0.05
      CALL PLCHMQ(XP,YP,MRKFILE,12.0,0.0,1.0)
c      XP=X2
c      YP=Y2+0.025
c      CALL PLCHMQ(XP,YP,TLAB,12.0,0.0,1.0)
      IXNAM = 'Kilometers East of Goodland '
      XP=0.5*(X1+X2)
      YP=Y1-0.05
      CALL PLCHMQ(XP,YP,IXNAM,12.0,0.0,0.0)
      IYNAM = 'Kilometers North of Goodland'
      XP=X1-0.0625
      YP=0.5*(Y1+Y2)
      CALL PLCHMQ(XP,YP,IYNAM,12.0,90.0,0.0)

C     Set up axes and do the plotting
C
      LSIZE = 15
      CALL MAJMIN(XMN,XMX,IFMTX,MJRX,MNRX,IPLX)
      CALL MAJMIN(YMN,YMX,IFMTY,MJRY,MNRY,IPLY)
      CALL SET(X1,X2,Y1,Y2,XMN,XMX,YMN,YMX,1)
      CALL LABMOD(IFMTX,IFMTY,IPLX,IPLY,LSIZE,LSIZE,8,8,0)
      CALL GRIDAL(MJRX,MNRX,MJRY,MNRY,1,1,5,X1,Y1)

      CSIZ = 12.0

      DO 30 I=1,IMRK
         XP=XMRK(I)
         YP=YMRK(I)
         
         IF(XP.LT.XMN .OR. XP.GT.XMX)GO TO 30
         IF(YP.LT.YMN .OR. YP.GT.YMX)GO TO 30
         write(6,1771)nmrk(i),xp,yp
 1771    format(6X,'PltNET: name,xy=',a8,2f10.3)
         
         WRITE(LAB9,21)NMRK(I)
 21      FORMAT(1X,A7,1X)
         CALL PLCHMQ (XP,YP,LAB9,CSIZ,0.0,-1.0)
         WRITE(LAB6,23)SMRK(1)
 23      FORMAT(A6)
         CALL PLCHHQ (XP,YP,LAB6,CSIZ,0.0,0.0)
         
 30   CONTINUE

      RETURN
      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE PLT_HIST(X,Y,MXHST,NPT,YD,XMN,XMX,YMN,YMX,ZREF,
     X     XSL,XSR,YSB,YST,SNDFILE,JLMA)
C
C     Plot a vertical histogram of X (percent) vs. Y (height)
C
C     MXHST - Maximum number of height bins
C     NPT   - Actual number of height bins
C     JLMA  - Number of LMA events with this time-space window
C
C     Abscissa (x):
C        X       - Values of variable to be plotted
C        IXNAM   - Name of the variable 
C        XMN,XMX - Minimum and maximum values for plotting
C
C     Ordinate (y):
C        Y       - Values of variable to be plotted
C        YMN,YMX - Minimum and maximum values for plotting
C
      CHARACTER*6 IFMTX,IFMTY
      CHARACTER*24 SNDFILE

      CHARACTER*14 IXNAM
      DIMENSION X(MXHST),Y(MXHST)
      
      X1=XSL
      X2=XSR
      Y1=YSB
      Y2=YST
      CALL SET(0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
      XP=X1
      YP=Y2+0.025
      CALL PLCHMQ(XP,YP,SNDFILE,12.0,0.0,-1.0)

C     Plot X vs. Y
C     
      LSIZE = 15
      CALL MAJMIN(XMN,XMX,IFMTX,MJRX,MNRX,IPLX)
      CALL MAJMIN(YMN,YMX,IFMTY,MJRY,MNRY,IPLY)
      CALL SET(X1,X2,Y1,Y2,XMN,XMX,YMN,YMX,1)
      CALL LABMOD(IFMTX,IFMTY,IPLX,IPLY,LSIZE,LSIZE,8,8,0)
      CALL GRIDAL(MJRX,MNRX,MJRY,MNRY,1,0,5,X1,Y1)

      DO 20 N=1,NPT
         XC=X(N)
         IF(XC.GT.XMX)THEN
            XC=XMX
c            CALL PLCHMQ (XC,YC,'*',10.0,0.0,0.0)
         END IF
         YC=Y(N)
         IF(XC.EQ.-999.0 .AND. YC.EQ.-999.0)GO TO 20
         IF(XC.EQ.0.0 .AND. YC.EQ.0.0)GO TO 20
         IF(XC.LT.XMN.OR.XC.GT.XMX)GO TO 20
         IF(YC.LT.YMN.OR.YC.GT.YMX)GO TO 20
         YT=YC+0.5*YD
         YB=YC-0.5*YD
         CALL LINE(XMN,YB,XC,YB)
         CALL LINE(XMN,YT,XC,YT)
         CALL LINE(XC,YB,XC,YT)
 20   CONTINUE

C     Draw ZREF reference thick, red dashed line
C
      CALL SFLUSH
      CALL GETUSV('LW',ILW)
      JLW=1800
      CALL SETUSV('LW',JLW)
      CALL GSPLCI (IRED)
      CALL DASHDB (O'170360')
c      CALL LINED(XMN,ZREF,XMX,ZREF)

C     Restore line thickness and color
C
      CALL SFLUSH
      CALL SETUSV('LW',ILW)
      CALL GSPLCI (1)
      
C     Label the current plot and return
C
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)

      XP=XSL+0.02
      YP=YST-0.02
c      IXNAM = 'Temperature'
      WRITE(IXNAM,25)JLMA
 25   FORMAT('Total=',I8)
      CALL PLCHMQ (XP, YP, IXNAM, 12.0, 0.0, -1.0)

      RETURN
      END
c     
c----------------------------------------------------------------------X
c
      SUBROUTINE PLT_LMA(XLMA,YLMA,ZLMA,TLMA,MXLM,ILMA,XMN,XMX,
     X     YMN,YMX,HMN,HMX,TS_MN,TS_MX,XL,XR,YB,YT,LMAFILE,TLAB)

C  Plot XY-locations of LMA lightning data.
C
C     XLMA,YLMA   - (X,Y) position (km) of LMA data
C     ZLMA,TLMA   - Time and height (km) of LMA data
C     ILMA        - Number of LMA data points
C     XMN,XMX     - X bounds of the plot domain
C     YMN,YMX     - Y bounds of the plot domain
C     HMN,HMX     - Height bounds for plotting
C     TS_MN,TS_MX - Time bounds for plotting
C
      CHARACTER*6 IFMTX,IFMTY
      CHARACTER LMAFILE*32,TLAB*18,ZLAB*38
      CHARACTER IXNAM*28,IYNAM*28

      DIMENSION XLMA(MXLM),YLMA(MXLM),ZLMA(MXLM)
      DIMENSION TLMA(MXLM)

      X1=XL
      X2=XR
      Y1=YB
      Y2=YT

C     Add labels for title and axes
C
      CALL SET(0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
      XP=X1
      YP=Y2+0.05
      CALL PLCHMQ(XP,YP,LMAFILE,12.0,0.0,-1.0)
      WRITE(ZLAB,11)NINT(HMN),NINT(HMX),TLAB
 11   FORMAT('[Zslab=',I2.2,'--',I2.2,' km]   ',A18)
      XP=X2
      YP=Y2+0.025
      CALL PLCHMQ(XP,YP,ZLAB,12.0,0.0,1.0)
      IXNAM = 'Kilometers East of Goodland '
      XP=0.5*(X1+X2)
      YP=Y1-0.05
      CALL PLCHMQ(XP,YP,IXNAM,12.0,0.0,0.0)
      IYNAM = 'Kilometers North of Goodland'
      XP=X1-0.0625
      YP=0.5*(Y1+Y2)
      CALL PLCHMQ(XP,YP,IYNAM,12.0,90.0,0.0)

C     Set up axes and do the plotting
C
      LSIZE = 15
      CALL MAJMIN(XMN,XMX,IFMTX,MJRX,MNRX,IPLX)
      CALL MAJMIN(YMN,YMX,IFMTY,MJRY,MNRY,IPLY)
      CALL SET(X1,X2,Y1,Y2,XMN,XMX,YMN,YMX,1)
      CALL LABMOD(IFMTX,IFMTY,IPLX,IPLY,LSIZE,LSIZE,8,8,0)
      CALL GRIDAL(MJRX,MNRX,MJRY,MNRY,1,1,5,X1,Y1)

C     X-Y plot of all LMA data points within the time-space window
C
      CSIZ = 10.0
      JLMA=0
      DO I=1,ILMA
         XP=XLMA(I)
         YP=YLMA(I)
         ZP=ZLMA(I)
         TP=TLMA(I)
         IF((TP .GE. TS_MN .AND. TP .LE. TS_MX) .AND.
     X      (XP .GE. XMN   .AND. XP .LE. XMX  ) .AND.
     X      (YP .GE. YMN   .AND. YP .LE. YMX  ) .AND.
     X      (ZP .GE. HMN   .AND. ZP .LE. HMX  ))THEN
            print *,'XY-LMA: T,X,Y,Z=',tp,xp,yp,zp
            CALL PLCHMQ(XP,YP,'*',6.0,0.0,0.0)
            JLMA=JLMA+1
         END IF
      END DO
      print *,'XY-JLMA: JLMA=',jlma

      RETURN
      END
c     
c----------------------------------------------------------------------X
c
      SUBROUTINE PLT_NLD(XNLD,YNLD,PNLD,TNLD,MXNL,INLD,XMN,XMX,
     X     YMN,YMX,TS_MN,TS_MX,XL,XR,YB,YT,NLDFILE,TLAB)

C  Plot XY-locations of NLD lightning data.
C
C     XNLD,YNLD   - (X,Y) position (km) of NLDN data
C     TNLD,PLND   - Time (sec) and polarity of NLDN data
C     INLD        - Number of NLDN data points
C     XMN,XMX     - X bounds of the plot domain
C     YMN,YMX     - Y bounds of the plot domain
C     TS_MN,TS_MX - Time bounds for plotting
C
      INCLUDE 'snding.inc'
      INCLUDE 'colors.inc'
      CHARACTER*6 IFMTX,IFMTY
      CHARACTER NLDFILE*32,TLAB*18
      CHARACTER IXNAM*28,IYNAM*28

      DIMENSION XNLD(MXNL),YNLD(MXNL),PNLD(MXNL),TNLD(MXNL)

      X1=XL
      X2=XR
      Y1=YB
      Y2=YT

C     Add labels for title and axes
C
      CALL SET(0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
      XP=X1
      YP=Y2+0.025
      CALL PLCHMQ(XP,YP,NLDFILE,12.0,0.0,-1.0)
      XP=X2
      YP=Y2+0.025
      CALL PLCHMQ(XP,YP,TLAB,12.0,0.0,1.0)
      IXNAM = 'Kilometers East of Goodland '
      XP=0.5*(X1+X2)
      YP=Y1-0.05
      CALL PLCHMQ(XP,YP,IXNAM,12.0,0.0,0.0)
      IYNAM = 'Kilometers North of Goodland'
      XP=X1-0.0625
      YP=0.5*(Y1+Y2)
      CALL PLCHMQ(XP,YP,IYNAM,12.0,90.0,0.0)

C     Set up axes and do the plotting
C
      LSIZE = 15
      CALL MAJMIN(XMN,XMX,IFMTX,MJRX,MNRX,IPLX)
      CALL MAJMIN(YMN,YMX,IFMTY,MJRY,MNRY,IPLY)
      CALL SET(X1,X2,Y1,Y2,XMN,XMX,YMN,YMX,1)
      CALL LABMOD(IFMTX,IFMTY,IPLX,IPLY,LSIZE,LSIZE,8,8,0)
      CALL GRIDAL(MJRX,MNRX,MJRY,MNRY,1,1,5,X1,Y1)

C     Change line thickness and text/line color
C
      CALL SFLUSH
      CALL GETUSV('LW',ILW)
      JLW=1800
      CALL SETUSV('LW',JLW)
      CALL GSPLCI (IRED)
      CALL GSTXCI(IRED)

C     X-Y plot of all NLD data points within the time-space window
C
      CSIZ = 15.0
      JNLD=0
      DO I=1,INLD
         TP=TNLD(I)
         XP=XNLD(I)
         YP=YNLD(I)
         POLAR=PNLD(I)
         IF((TP .GE. TS_MN .AND. TP .LE. TS_MX) .AND.
     X      (XP .GE. XMN   .AND. XP .LE. XMX  ) .AND.
     X      (YP .GE. YMN   .AND. YP .LE. YMX  ))THEN
            print *,'XY-NLD: T,X,Y=',tp,xp,yp
            IF(POLAR.GT.0.0)THEN
               CALL PLCHMQ (XP,YP,'X',CSIZ,0.0,0.0)
            ELSE
               CALL PLCHMQ (XP,YP,'0',CSIZ,0.0,0.0)
            END IF
            JNLD=JNLD+1
         END IF
      END DO
      print *,'XY-JNLD: JNLD=',jnld         

C     Restore line thickness
C
      CALL SFLUSH
      CALL SETUSV('LW',ILW)
      CALL GSPLCI (1)
      CALL GSTXCI(1)

      RETURN
      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE REF_GRID

      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PERIM(1,1,1,1)
      Y1=0.0
      Y2=1.0
      DO I=1,9
         X1=0.1*I
         X2=0.1*I
         CALL LINE(X1,Y1,X2,Y2)
      END DO
      X1=0.0
      X2=1.0
      DO I=1,9
         Y1=0.1*I
         Y2=0.1*I
         CALL LINE(X1,Y1,X2,Y2)
      END DO
      RETURN
      END
