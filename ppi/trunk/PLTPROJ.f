c
c----------------------------------------------------------------------X
c
      SUBROUTINE PLTPROJ(JNDAT,ITIMBOV,ITIMEOV,FXVOL,NFXVOL,NFXMAX,
     X     NETWORK,IRATYP,ICORD,IVOLOLD,NFRAME,JTP,JMAP,JACT,JMRK)
C
C  PLOT HORIZONTAL (VERTICAL) PROJECTION OF RHI (PPI or SUR) FIXED ANGLE 
C     LOCATIONS WITH OVERLAYS
C
C     XMN,XMX - MIN/MAX PLOT BOUNDARIES FOR E-W HORIZONTAL DISTANCE (KM)
C     YMN,YMX -    "      "       '      "  N-S     "          "      "
C               Note: When projecting SUR or PPI scans YMN,YMX is height.
C     RMN,RMX - MIN/MAX HORIZONTAL RANGES TO BE PLOTTED
C     OVERLAY FLAGS:
C       JMAP  = (0) NO MAP OVERLAY, (1) SOLID LINE, (2) DOTTED LINE
C       JMRK  = (0) NO LANDMARKS, (1) NAMES, (2) NAMES IN COLORED BOXES
C       JACT  = (0) NO AIRCRAFT TRACK, 
C               (1) PLOT ONLY AIRCRAFT TRACK, 
C               (2) PLOT ONLY TIME SERIES INSERT,
C               (3) PLOT BOTH AIRCRAFT TRACK AND TIME SERIES INSERT
C      NFXVOL - NUMBER OF FIXED ANGLES IN THE CURRENT VOLUME SCAN
C
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      INCLUDE 'colors.inc'
      INCLUDE 'swth.inc'

      CHARACTER LAB*80,LABLS*3,IFMTX*6,IFMTY*6
      CHARACTER*8 JNDAT(10),NETWORK,IRATYP,ICORD
      CHARACTER*8 ISCTP(8),PROJTYP
      CHARACTER*1 BGFLAG
      LOGICAL COLRFIL

      COMMON/ORIGIN/X0,Y0,H0,AZCOR,BAZ,XRD,YRD
      COMMON /PLTWIN/XRT(26),YTP(26),SIDEX(26),SIDEY(26),DXL,DXR,DYB,
     + DYT,NROW,NCOL,NWIN,IWIN,LABX(26),LABY(26),ILFLG,XBTP(26),
     + YBTP(26),SIZBX,XSHIFT,YSHIFT

      DATA ISCTP/'PPI','COP','RHI','VER','TAR','MAN','IDL','SUR'/
      DATA RE,REI/17000.0,1.17647E-04/
      DATA TORAD,TODEG/0.017453293,57.29577951/

      DIMENSION FXVOL(NFXMAX)

      READ(JNDAT,5)PROJTYP,XMN,XMX,YMN,YMX,RMN,RMX,JMAP,JACT,JMRK,ZSTR
 5    FORMAT(/A8/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/I1,I1,I1/F8.0)
      IF(ZSTR.EQ.0.0)ZSTR=1.0
      IF(PROJTYP.NE.'HOR' .AND. PROJTYP.NE.'VER')THEN
         WRITE(*,*)'***ONLY HOR AND VER PROJECTIONS ALLOWED***'
         RETURN
      END IF
      IF(PROJTYP.EQ.'HOR')THEN
         JTP=IFIND('SUR     ',ISCTP,8)
      ELSE IF(PROJTYP.EQ.'VER')THEN
         JTP=IFIND('RHI     ',ISCTP,8)
      END IF
      if (jtp.eq.0)jtp=9
      ITPSAV=ITPOLD
      ITPOLD=JTP

      write(6,1770)itimbov,itimeov,nfxvol,jtp,xmn,xmx,ymn,ymx,
     +     rmn,rmx,jmap,jact,jmrk
 1770 format(1x,' pltproj: ',i6.6,'-',i6.6,2i6,6f8.1,3i1)

      CALL SFLUSH
      CALL GSCR(1,0,0.,0.,0.)
      CALL GSCR(1,1,1.,1.,1.)
      CALL GSPLCI(1)
      CALL GSTXCI(1)

C     Initialize plotting frame parameters
C     Note: Use SETWIN with NROW=0.0 before PLTPROJ command.
C
      COLRFIL=.FALSE.
      IGRPLT=0
      BGFLAG=' '

      XMIN(JTP)=XMN
      XMAX(JTP)=XMX
      YMIN(JTP)=YMN
      YMAX(JTP)=YMX
      GXMIN(JTP)=XMN
      GXMAX(JTP)=XMX
      GYMIN(JTP)=YMN
      GYMAX(JTP)=YMX

      CALL INIT1(ZSTR,COLRFIL,XRT(IWIN),YTP(IWIN),SIDEX(IWIN),
     X     SIDEY(IWIN),IGRPLT,BGFLAG,LABX(IWIN),LABY(IWIN),
     X     DXL,DXR,DYB,DYT,NROW,NCOL,XSHIFT,YSHIFT)

      GXMN=GXMIN(JTP)
      GXMX=GXMAX(JTP)
      GYMN=GYMIN(JTP)
      GYMX=GYMAX(JTP)

      SINC=SIN(TORAD*AZCOR)
      COSC=COS(TORAD*AZCOR)

      JD=1
      ID=1
      IF(DRSW.NE.0.0)THEN
         ID=NINT(1.0/DRSW)
      END IF

      write(6,1771)iwin,azcor,nfxvol,jd,mngate,mxgate,id,
     +     rng(mngate,2),rng(mxgate,2),drsw
 1771 format(1x,' pltproj: ',i8,f8.1,5i8,2f8.1,f8.3)

      DO 150 J=1,NFXVOL,JD
         SINA=SIN(TORAD*FXVOL(J))
         COSA=COS(TORAD*FXVOL(J))

         DO 130 I=MNGATE,MXGATE,ID

            IF(PROJTYP.EQ.'HOR')THEN
               X1=RNG(I,2)*SINA
               Y1=RNG(I,2)*COSA
               IF(ICVRT)THEN
                  XPT=(X1*COSC-Y1*SINC)+X0
                  YPT=(X1*SINC+Y1*COSC)+Y0
               ELSE
                  XPT=X1+X0
                  YPT=Y1+Y0
               END IF
               YPN=YPT
            ELSE IF(PROJTYP.EQ.'VER')THEN
               HRNG=RNG(I,2)*COSA
               XPT=HRNG
               YPT=H0+RNG(I,2)*SINA+0.5*HRNG*HRNG*REI
C               YPN=H0+RNG(I,2)*SINA
c--------------print *,'PROJ: j,i,xpt,ypt,ypn=',j,i,xpt,ypt,ypn
            END IF

            IF(XPT.LT.XMIN(JTP).OR.XPT.GT.XMAX(JTP).OR.
     X         YPT.LT.YMIN(JTP).OR.YPT.GT.YMAX(JTP))GO TO 130

            IF((XPT.GE.GXMN .AND. XPT.LE.GXMX).AND.
     X         (YPT.GE.GYMN .AND. YPT.LE.GYMX))THEN
               CALL PLCHMQ(XPT,YPT,'X',1.0,0.0,0.0)
C               CALL PLCHMQ(XPT,YPN,'X',1.0,0.0,0.0)
            END IF

 130     CONTINUE
 150  CONTINUE

      ITPOLD=ITPSAV
      RETURN
      END




