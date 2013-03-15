c     
c----------------------------------------------------------------------X
c
      SUBROUTINE PLTNLD(XNLD,YNLD,PNLD,TNLD,INLD,JNLD,MXNL,DTNLD,
     X     IBTIME,IETIME,OLAT,OLON,ANGXAX,CNLD,DZNLD,ZLEV,INLDDEF,
     X     THKNLD,LABFLG,ITIT,PWIND,NCWORD)
C
C  Plot NLD CG lightning positions overlaid on CONTOUR or COLOR plots, only
c     if the strike is within the plot space (XMN,XMX,YMN,YMX) and middle
C     time of the radar scan.
C     Color fill a box and plot the polarity of the strike inside it.
C
C     XMN,XMX,YMX,YMX - (X,Y) BOUNDS OF THE PLOT DOMAIN
C     INLD      - Number of strikes read in (See GETNLD).
C     JNLD      - (0) no CG polarity, (1) CG polarity
C     DTNLD     - Time interval for plotting strikes.  Plot points within
C                 a time window relative to the current radar scan.
C                 DTNLD .gt. 0: Plot +/- DTNLD from central time (RMSEC)
C                 DTNLD .le. 0: Plot within DTNLD seconds outside the
C                               volume time interval
C     THKNLD    - Line thickness multiplier for NLDN location plot
C     IBTIME    - Beginning time of the volume
C     IETIME    - Ending time of volume
C
C     XNLD,YNLD - (X,Y) position (km) of CG strike relative to origin
C     TNLD      - Time of the strike (sec)
C     PNLD      - Polarity of ground strike and E-field strength (Kv/m)
C
      INCLUDE 'CEDRIC.INC'
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X     IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X     NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      COMMON /RANGEC/ XBEG,XEND,YBEG,YEND,XRANGE(2),YRANGE(2),DELRG(2)
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      CHARACTER*4 AXNAM
      CHARACTER*1 IAXIS(3)
      CHARACTER*16 CFMTX,CFMTY
      CHARACTER*8 NOW,IFMTX(2),IFMTY(2),ITIT(5)
      CHARACTER*80 CITIT
      CHARACTER*80 JTIT
      CHARACTER*1 BLANK
      PARAMETER (BLANK=' ')
      DATA IAXIS/'X','Y','Z'/
      DIMENSION PWIND(2,3),NCWORD(3)

      DIMENSION XNLD(MXNL),YNLD(MXNL),TNLD(MXNL),PNLD(MXNL)
      DATA XSIZ/12.0/

C     Function: convert fractional (0-1) to raster addressing (0-1023)
C
      LOCPLT(R)=1023.*R

      CALL GSFAIS(1)

      L1=NCWORD(1)
      L2=NCWORD(2)
      L3=NCWORD(3)
      CALL RGINI(FL,FR,FB,FT,PWIND,CSP,NCX,NCWORD)
      print *,'PLTNLD: l1,l2,l3=',l1,l2,l3
      print *,'     fl,fr,fb,ft=',fl,fr,fb,ft
      XL=FL
      XR=FR
      YB=FB
      YT=FT

C     DTNLD .le. 0: Plot within DTNLD of central time (RMSEC).
C     DTNLD .gt. 0:             DTNLD of begin-end times
C
      RMSEC=0.5*(IETIME+IBTIME)
      IF(DTNLD.GT.0.0)THEN
         SECMN=RMSEC-DTNLD
         SECMX=RMSEC+DTNLD
      ELSE
         SECMN=FLOAT(IBTIME)-ABS(DTNLD)
         SECMX=FLOAT(IETIME)+ABS(DTNLD)
      END IF

      IF(CNLD.LE.0.0)THEN
         CSIZ=XSIZ
      ELSE
         CSIZ=CNLD
      END IF

C     Save the plotting instructions into a FLASH buffer
C
      CALL GFLAS1(5)
      CALL GSCLIP(0)
      CALL SET(FL,FR,FB,FT,PWIND(1,L1),PWIND(2,L1),PWIND(1,L2),
     X         PWIND(2,L2),1)
      XMN=PWIND(1,L1)
      XMX=PWIND(2,L1)
      YMN=PWIND(1,L2)
      YMX=PWIND(2,L2)
      ZMN=PWIND(1,L3)
      ZMX=PWIND(2,L3)
      UL=XMN
      UR=XMX
      UB=YMN
      UT=YMX

      print *,'PLTNLD: ibtime,ietime=',ibtime,ietime
      print *,'              xmn,xmx=',xmn,xmx
      print *,'              ymn,ymx=',ymn,ymx
      print *,'              zmn,zmx=',zmn,zmx
      print *,'          secmn,secmx=',secmn,secmx
      print *,'                 csiz=',csiz

C     Get default line thickness (ILW) and reset to JLW.
C     Restore line thickness before leaving routine.
C
      CALL GETUSV('LW',ILW)
      JLW=THKNLD*ILW
      IF(JLW.LT.ILW)JLW=ILW
      CALL SETUSV('LW',JLW)
      print *,'PLTNLD: thknld,ilw,jlw=',thknld,ilw,jlw

C     JLND = 1: PLOT SYMBOL [polarity = + (X), - (O)]
C
      DO 30 I=1,INLD

         XN=XNLD(I)
         YN=YNLD(I)
         TN=TNLD(I)
         POLAR=PNLD(I)

         IF(XN.LT.XMN .OR. XN.GT.XMX)GO TO 30
         IF(YN.LT.YMN .OR. YN.GT.YMX)GO TO 30
         IF(TN.LT.SECMN .OR. TN.GT.SECMX)GO TO 30
         print *,'I,x,y,t,polar=',I,xn,yn,tn,polar
         IF(POLAR.GT.0.0)THEN
            CALL PLCHMQ (XN,YN,'X',CSIZ,0.0,0.0)
         ELSE
            CALL PLCHMQ (XN,YN,'0',CSIZ,0.0,0.0)
         END IF
 30   CONTINUE

      CALL SETUSV('LW',ILW)
      CALL GFLAS2

C     Finished defining NLDN location plot with 
C     instructions stored in FLASH buffer #5.

      IF (INLDDEF.EQ.1) THEN

C     INLDDEF - (0) Plot in PLOTCH from FLASH buffer #5.
C               (1) Plot here as a separate labeled frame.
C     GSCLIP(0) - turn off clipping to allow labels
C                 and such outside the plot window
         CALL GSCLIP(0)
         WRITE (CITIT(1:80),500)(ITIT(I),I=1,5),BLANK
 500     FORMAT(5A8,39A1,'X')
         print *,'PLTNLD: itit =',itit
         print *,'PLTNLD: citit=',citit
         CALL DATEE(NOW)
         CF=1./ID(69)
         XOR=ID(40)*CF
         SF=1./ID(68)
         XREL=ID(41)*SF
         YREL=ID(42)*SF
         CALL MAJMIN(XRANGE(1),DELRG(1),IFMTX,MAJORX,MINORX,NDIG1,ISZ1,
     X               YRANGE(1),DELRG(2),IFMTY,MAJORY,MINORY,NDIG2,ISZ2)
C         CALL MAJMIN(DELRG(1),IFMTX,MAJORX,MINORX)
C         CALL MAJMIN(DELRG(2),IFMTY,MAJORY,MINORY)
         print *,'PLTNLD: x=',xrange(1),delrg(1),ifmtx(1),majorx,minorx
         print *,'PLTNLD: y=',yrange(1),delrg(2),ifmty(1),majory,minory
         WRITE (CFMTX,510)IFMTX
 510     FORMAT(2A8)
         WRITE (CFMTY,510)IFMTY
         print *,'CFMTX,CFMTY=',cfmtx(1:8),cfmty(1:8)

C     Always draw grid and grid labels
         CALL LABMOD(CFMTX,CFMTY,NDIG1,NDIG2,ISZ1,ISZ2,4,4,0)
         CALL PERIML(MAJORX,MINORX,MAJORY,MINORY)

C     JUST DRAW A BOX AROUND PLOT-NO TICK MARKS
c            CALL TICKS(0,0)
c            CALL PERIM(0,0,0,0)
c            CALL TICKS(12,8)
            
         WRITE (JTIT,106)(ID(I),I=116,121),(ID(I),I=125,127),
     X        (ID(I),I=13,15),AXNAM(L3),ZLEV,
     X        LABAXS(L3,IUNAXS)
 106     FORMAT(I2.2,'/',I2.2,'/',I2.2,6X,I2.2,2(':',I2.2),'-',
     X        I2.2,2(':',I2.2),7X,3A2,7X,A2,'=',F7.2,' ',A4)
         IF (LABFLG.GT.5) THEN
            CALL MY_PLCHMQ(60,1010,JTIT(1:66),12.,0.,-1.)
            WRITE (JTIT,107)NOW
 107        FORMAT('(AS OF ',A8,')')
            CALL MY_PLCHMQ(10,985,JTIT(1:16),12.,0.,-1.)
            WRITE (JTIT,105)XREL,YREL,XOR
 105        FORMAT('ORIGIN=(',F7.2,',',F7.2,') KM   X-AXIS=',F5.1,
     X           ' DEG')
            CALL MY_PLCHMQ(430,985,JTIT(1:46),12.,0.,-1.)
            CALL MY_PLCHMQ(200,960,CITIT,12.,0.,-1.)
            WRITE (JTIT,108)AXNAM(L2),LABAXS(L2,IUNAXS)
 108        FORMAT(A1,' ',A4)
         END IF
         LOCY=LOCPLT(YB+(YT-YB)*0.15)
         IF (LABFLG.GT.5) THEN
            CALL MY_PLCHMQ(10,LOCY,JTIT(1:6),12.,90.,-1.)
            WRITE (JTIT,108)AXNAM(L1),LABAXS(L1,IUNAXS)
         END IF
         LOCY=LOCPLT(YB)-50
         IF (LABFLG.GT.5) THEN
            CALL MY_PLCHMQ(150,LOCY,JTIT(1:6),12.,0.,-1.)
         END IF
         
C        Add the actual NLDN location plot from the FLASH buffer
C
         CALL GFLAS3(5)
         IF (LABFLG.GT.5) THEN
            CALL MYFRAME
         ELSE
            CALL FRAME
         END IF
      END IF

      RETURN
      END



