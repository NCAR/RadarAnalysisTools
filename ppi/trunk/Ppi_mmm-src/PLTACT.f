c
c----------------------------------------------------------------------X
c
      SUBROUTINE PLTACT(XACT,YACT,ZACT,UACT,VACT,WACT,CACT,QACT,DACT,
     X     HACT,TACT,DTAC,DPAC,IACT,JACT,XMN,XMX,YMN,YMX,IBTIME,IETIME,
     X     X0,Y0,H0,FXOLD,ITPOLD,IGRPLT,BGFLAG,TMJR,TMNR,TS_LL,TS_SIZ,
     X     VECSCL,SCLR,WTYM,WMN,WMX,MXL,LABLS,IBLACK,IWHITE,NWIN,
     X     FXANG1,FXANG2,MXR,MXA,MNGATE,MXGATE,MANG,AZA,ELA,R0,DROLD)
C
C  PLOT AIRCRAFT POSITIONS OVERLAID ON CONTOUR OR COLOR PLOTS, ONLY
C     IF THE AIRCRAFT POSITION (X,Y,T) IS WITHIN THE PLOT SPACE
C     (XMN,XMX,YMN,YMX) AND TIME (IMTIME-DTAC,IMTIME+DTAC) WINDOW
C     Note: When the scan is an RHI, the position is projected into the
C           plane of the azimuth (FXOLD) angle being scanned.  For that
C           case "X" is range and the "Y" is height.
C     Note: Plot aircraft track from IBTIME-ABS(DTAC) to IETIME+ABS(DTAC)
C           when plotting onto projection (PLTPROJ).
C  
C  Plot (B) beginning time of overlaid track, (M) middle time of overlaid
C     track, and (X) plotted every min for horizontal projections (PLTPROJ).
C     Set (HRC,AZC) to first point (FIRST_PT) true horizontal range and 
C     azimuth for annotating the plots AIRCRAFT [HRC/AZC].
C
C     JACT = (0) NO AIRCRAFT TRACK, 
C            (1) PLOT ONLY AIRCRAFT TRACK, 
C            (2) PLOT ONLY TIME SERIES INSERT,
C            (3) PLOT BOTH AIRCRAFT TRACK AND TIME SERIES INSERT
C
C     XACT,YACT,ZACT,TACT  - AIRCRAFT POSITION (KM,KM,KM,SEC)
C     UACT,VACT,WACT       - (U,V,W)
C     CACT,QACT,DACT       - FSSP CONC, LWC, and DBZ (#/cc, g/m3, dBZ)
C     HACT                 - AZIMUTH TOWARD WHICH THE AIRCRAFT IS HEADED
C     IACT                 - NUMBER OF AIRCRAFT POSITIONS
C     (DTAC,DPAC)          - TIME WINDOW (RADAR CENTRAL TIME +/- DTAC SEC)
C                            AND SPACE WINDOW (RADAR SCAN +/- DPAC KM)
C     (TMJR,TMNR)          - Plot circled (+) every TMJR sec and
C                            wind vector every TMNR sec.
C     WTYM                 _ Plot interval (sec) for time series insert.
C     TS_LL                - FRACTIONAL LOWER-LEFT CORNER FOR W INSERT
C     TS_SIZ               - FRACTIONAL XY SIZES FOR W INSERT
C     VECSCL               - WIND VECTOR PLOTTING SCALE (M/S PER KM)
C     SCLR                 - SCALAR PLOTTING SCALE (UNITS OF SCALAR PER KM)
C     XMN,XMX,YMX,YMX      - (X,Y) BOUNDS OF THE PLOT DOMAIN
C     IBTIME,IETIME        - TIME INTERVAL OF THE RADAR SCAN (HHMMSS)
C     X0,Y0,HO             - RADAR POSITION (KM,KM,KM)
C     FXOLD                - FIXED ANGLE (ELEVATION OR AZIMUTH) OF SCAN 
C                            BEING PLOTTED
C     ITPOLD               - SCAN MODE [PPI(1), COP(2), RHI(3), or SUR(8)]
C     FXANG1,FXANG2        - Minimum, maximum fixed angles for swaths.
C     NPLOTTED             - Number of aircraft positions that were plotted.
C                            If none, return without labeling the plot.
C
      CHARACTER LAB6*6,LABB*1,LABT*6,LABM*3,BGFLAG*1,ACFILT*4,LABLS*3
      CHARACTER LABAC*80
      CHARACTER*8 TS_LL,TS_SIZ
      LOGICAL FIRST_PT
      LOGICAL DECR
      REAL FIRST,LAST

      DIMENSION AZA(MXA,2),ELA(MXA,2)
      DIMENSION XACT(MXL),YACT(MXL),ZACT(MXL),TACT(MXL)
      DIMENSION UACT(MXL),VACT(MXL),WACT(MXL)
      DIMENSION CACT(MXL),QACT(MXL),DACT(MXL),HACT(MXL)
      DATA TORAD,TODEG/0.017453293,57.29577951/
      DATA LAB6/'&KGL&E'/
      DATA IGRAY,IRED,IGREEN,IBLUE/65,66,67,68/
      DATA ICYAN,IMAGENTA,IYELLOW/69,70,71/
      DATA CONCMIN,DANGL/50.0,1.5/
      DATA EPS/0.5/
      DATA FX,FY/0.01,0.01/

C     Don't plot if the aircraft time segment [TACT(1) to TACT(IACT)]
C     is completely outside the plotting time segment [SECMN to SECMX]. 
C        IBTIME - Beginning  time (HHMMSS) of the radar scan
C        IMTIME - Middle     time (HHMMSS) of the radar scan
C        IETIME - Ending     time (HHMMSS) of the radar scan
C        IBSEC  - Beginning  time (sec) of the radar scan
C        IMSEC  - Middle     time (sec) of the radar scan
C        IESEC  - Ending     time (sec) of the radar scan
C        SECMN  - Earliest time (sec) for plotting track
C        SECMX  - Latest   time (sec) for plotting track
C
      IHRB= IBTIME/10000
      IMNB=(IBTIME-IHRB*10000)/100
      ISCB= IBTIME-IHRB*10000-IMNB*100
      IBSEC=IHRB*3600+IMNB*60+ISCB

      IHRE= IETIME/10000
      IMNE=(IETIME-IHRE*10000)/100
      ISCE= IETIME-IHRE*10000-IMNE*100
      IESEC=IHRE*3600+IMNE*60+ISCE

      IMSEC=0.5*(IBSEC+IESEC)
      IHRM= IMSEC/3600.0
      IMNM= (IMSEC-3600.0*IHRM)/60.0
      ISCM= IMSEC-3600.0*IHRM-60.0*IMNM
      RMSEC=FLOAT(IHRM*3600+IMNM*60+ISCM)
      IMTIME=IHRM*10000+IMNM*100+ISCM

C     Plot +/- DTAC from midtime of individual scans or
C     from VolBegTime-DTAC to VolEndTime+DTAC for PLTPROJ.
C
      IF(DTAC.GT.0.0)THEN
         SECMN=RMSEC-DTAC
         SECMX=RMSEC+DTAC
      ELSE
         SECMN=FLOAT(IBSEC)-ABS(DTAC)
         SECMX=FLOAT(IESEC)+ABS(DTAC)
      END IF

      write(6,1770)ibtime,imtime,ietime,ibsec,imsec,iesec
 1770 format(1x,'PLTACT: bme(sec)=',6i8)
      write(6,1771)nint(secmn),nint(secmx)
 1771 format(1x,'       radar sec=',2i8)
      write(6,1772)nint(tact(1)),nint(tact(iact))
 1772 format(1x,'        acft sec=',2i8)
c      print *,'PLTACT: xmn,xmx,ymn,ymx=',xmn,xmx,ymn,ymx

      IF(SECMX.LE.TACT(1))RETURN
      IF(SECMN.GE.TACT(IACT))RETURN
      print *,'Aircraft times overlap radar times'

      NPLOTTED=0

C     Change line widths and will set some colors for lines and text
C        IGRPLT - (0) Rainbow   use some colors for lines and text
C                 (1) Graytones  "  foreground   "    "    "    "
C
      JLW=2000
      CALL GETUSV('LW',ILW)
      CALL SETUSV('LW',JLW)
      FIRST_PT=.TRUE.

C     Plot winds every ITYM sec and circled (+) every MJRPT sec.
C
      ITYM=NINT(TMNR)
      MJRPT=NINT(TMJR)

      CALL DASHDB (O'177777')
C     CALL DASHDB (O'170360')
C     CALL DASHDB (O'070707')

C     Determine if radar scanning in increasing (decreasing) angle direction
C
      FIRST=AZA(1,1)
      LAST =AZA(MANG,1)
      IF(FIRST.GT.LAST)THEN
         DECR=.TRUE.
      ELSE
         DECR=.FALSE.
      END IF

C     Calculate bounds of radar scan.
C
      RMIN=R0+DROLD*(MNGATE-1)
      RMAX=R0+DROLD*(MXGATE-1)

C     RHI: AZA contains elevation angles scanned.
C
      IF(ITPOLD.EQ.3)THEN

         IF(DECR)THEN
            ELMIN=AZA(MANG,1)
            ELMAX=AZA(1,1)
            IF(ELMIN.GT.180.0)ELMIN=ELMIN-360.0
            IF(ELMAX.GT.180.0)ELMAX=ELMAX-360.0
         ELSE
            ELMIN=AZA(1,1)
            ELMAX=AZA(MANG,1)
            IF(ELMIN.GT.180.0)ELMIN=ELMIN-360.0
            IF(ELMAX.GT.180.0)ELMAX=ELMAX-360.0
         END IF

         DE=(ELMAX-ELMIN)/(MANG-1)
         DA=DANGL
         AZMIN=FXOLD-DA
         AZMAX=FXOLD+DA

C     Otherwise: AZA contains azimuth angles scanned.
C
      ELSE

         IF(DECR)THEN
            AZMIN=AZA(MANG,1)
            AZMAX=AZA(1,1)
         ELSE
            AZMIN=AZA(1,1)
            AZMAX=AZA(MANG,1)
         END IF

         DA=(AZMAX-AZMIN)/(MANG-1)
         DE=DANGL
         ELMIN=FXOLD-DE
         ELMAX=FXOLD+DE

      END IF

C     USE VALUES PROJECTED INTO AZIMUTH-PLANE WHEN RHI SCAN MODE.
C
      IF(JACT.EQ.1.OR.JACT.EQ.3)THEN
         SINA=SIN(TORAD*FXOLD)
         COSA=COS(TORAD*FXOLD)
         
         DO 20 I=2,IACT-1

            TC=TACT(I)
            IF(TC.LT.SECMN .OR. TC.GT.SECMX)GO TO 20
            IHR= TACT(I)/3600.0
            IMN=(TACT(I)-IHR*3600.0)/60.0
            ISC= TACT(I)-IHR*3600.0-IMN*60.0
            ITACT=INT(TACT(I))

C           Calculate aircraft position relative to radar
C
            X=XACT(I)-X0
            Y=YACT(I)-Y0
            Z=ZACT(I)-H0
            HRNG=SQRT(X*X+Y*Y)
            SRNG=SQRT(X*X+Y*Y+Z*Z)
            IF(X.EQ.0.0 .AND. Y.EQ.0.0)THEN
               AZ=0.0
            ELSE IF(X.GT.0.0 .AND. Y.EQ.0.0)THEN
               AZ=90.0
            ELSE IF(X.EQ.0.0 .AND. Y.LT.0.0)THEN
               AZ=180.0
            ELSE IF(X.LT.0.0 .AND. Y.EQ.0.0)THEN
               AZ=270.0
            ELSE IF(X.NE.0.0 .AND. Y.NE.0.0)THEN
               AZ=TODEG*ATAN2(X,Y)
            ELSE
               AZ=0.0
            END IF
            IF(AZ.LT.0.0)AZ=AZ+360.0
            IF(HRNG.GT.0.0)THEN
               EL=TODEG*ATAN2(Z,HRNG)
            ELSE
               GO TO 20
            END IF

C           Check aircraft positions against radar data bounds.
C
            IF(SRNG.LT.RMIN .OR. SRNG.GT.RMAX)GO TO 20
c            IF(AZ.LT.AZMIN  .OR. AZ.GT.AZMAX )GO TO 20
c            IF(EL.LT.ELMIN  .OR. EL.GT.ELMAX )GO TO 20

C           RHI: project aircraft (x,y) into constant azimuth plane.
C
            IF(ITPOLD.EQ.3)THEN

               XR=XACT(I-1)-X0
               YR=YACT(I-1)-Y0
               X1= XR*SINA+YR*COSA
               Y1=-XR*COSA+YR*SINA
               Z1=ZACT(I-1)

               XR=XACT(I+1)-X0
               YR=YACT(I+1)-Y0
               X2= XR*SINA+YR*COSA
               Y2=-XR*COSA+YR*SINA
               Z2=ZACT(I+1)

               XR=XACT(I)-X0
               YR=YACT(I)-Y0
               XC= XR*SINA+YR*COSA
               YC=-XR*COSA+YR*SINA
               ZC=ZACT(I)

               IF(DTAC.LT.0.0 .and. (AZ.LT.FXANG1.OR.AZ.GT.FXANG2))THEN
                  GO TO 20
               END IF
               IF(XC.LT.XMN  .OR. XC.GT.XMX)GO TO 20
               IF(ZC.LT.YMN  .OR. ZC.GT.YMX)GO TO 20
               
               IHR= TACT(I)/3600.0
               IMN=(TACT(I)-IHR*3600.0)/60.0
               ISC= TACT(I)-IHR*3600.0-IMN*60.0
               IF(YC.GT.0.0)THEN
                  CALL PLCHMQ (XC,ZC,'X',1.0,0.0,0.0)
               ELSE
                  CALL LINE(X1,Z1,X2,Z2)
               END IF
               
C     Plot winds only if within DPAC km of plane of current scan
C     
               IF(ABS(YC).LE.DPAC)THEN
                  IF(ITYM.NE.0.AND.MOD(ITACT,ITYM).EQ.0)THEN
                     IF(VECSCL.GT.0.0)THEN
                        W1=ZC+WACT(I)/VECSCL

c-----------------------Special version for 1998 Cloud Physics Conference
c                       replace colored lines with foreground
c-----------------------IF(CACT(I) .GT. CONCMIN)THEN
c                          IF(IGRPLT.EQ.0)THEN
c                             CALL SFLUSH
c                             CALL GSPLCI(IRED)
c                             CALL GSTXCI(IRED)
c                             CALL LINE(XC,ZC,XC,W1)
c                             CALL SFLUSH
c                             CALL GSPLCI(1)
c                             CALL GSTXCI(1)
c                          ELSE
c                             CALL LINE(XC,ZC,XC,W1)
c                          END IF
c                       ELSE
c                           CALL LINE(XC,ZC,XC,W1)
c-----------------------END IF

                        IF(CACT(I) .GT. CONCMIN)CALL LINE(XC,ZC,XC,W1)

                     END IF
                  END IF
                  IF(SCLR.GT.0.0)THEN
                     SXL=X1
                     SZL=Z1+CACT(I-1)/SCLR
                     SXC=XC
                     SZC=ZC+CACT(I)/SCLR
                     CALL PLCHMQ (SXC,SZC,'X',1.0,0.0,0.0)
                     CALL LINE (SXL,SZL,SXC,SZC)
                  END IF
               END IF
               XP=XC
               YP=ZC

C           PPI/SUR: plot aircraft (x,y) as is.
C
            ELSE

               X1=XACT(I-1)
               Y1=YACT(I-1)
               Z1=ZACT(I-1)
               X2=XACT(I+1)
               Y2=YACT(I+1)
               Z2=ZACT(I+1)
               XC=XACT(I)
               YC=YACT(I)
               ZC=ZACT(I)
               TC=TACT(I)
               IF(X1.LT.XMN  .OR. X1.GT.XMX)GO TO 20
               IF(XC.LT.XMN  .OR. XC.GT.XMX)GO TO 20
               IF(X2.LT.XMN  .OR. X2.GT.XMX)GO TO 20
               IF(Y1.LT.YMN  .OR. Y1.GT.YMX)GO TO 20
               IF(YC.LT.YMN  .OR. YC.GT.YMX)GO TO 20
               IF(Y2.LT.YMN  .OR. Y2.GT.YMX)GO TO 20
               XL=XC-X0
               YL=YC-Y0
               ZL=ZC-H0
               HL=SQRT(XL*XL+YL*YL)
               IF(HL.GT.0.0)THEN
                  EL=TODEG*ATAN2(ZL,HL)
               ELSE
                  EL=FXOLD
               END IF
               IF(FIRST_PT)THEN
c                  CALL PLCHMQ (XC,YC,'B',15.0,0.0,0.0)
                  CALL PLCHMQ (XC,YC,'T',15.0,0.0,0.0)
                  HRC=HRNG
                  AZC=AZ
                  HEAD=HACT(I)
                  ALT=ZACT(I)
                  FIRST_PT=.FALSE.
               END IF

C              Plot small X's if below current scan; otherwise, plot line.
C
               IF(EL.LT.FXOLD)THEN
                  CALL PLCHMQ (XC,YC,'X',1.0,0.0,0.0)
               ELSE
                  CALL LINE(X1,Y1,X2,Y2)
               END IF

C              Plot C's if within 2 sec of middle of scan time.
C
               IF(ABS(TC-IMSEC) .LE. 2.0)THEN
                  CALL PLCHMQ (XC,YC,'C',10.0,0.0,0.0)
               END IF

               IF(ITYM.NE.0)THEN
                  IF(MOD(ITACT,ITYM).EQ.0)THEN
                     IF(VECSCL.GT.0.0)THEN
                        U1=XC+UACT(I)/VECSCL
                        V1=YC+VACT(I)/VECSCL

c--------------------Special version for 1998 Cloud Physics Conference
c                    replace colored lines with foreground
c                        IF(CACT(I) .GT. CONCMIN)THEN
c                           IF(IGRPLT.EQ.0)THEN
c                              CALL SFLUSH
c                              CALL GSPLCI(IRED)
c                              CALL GSTXCI(IRED)
c                              CALL LINE(XC,YC,U1,V1)
c                              CALL SFLUSH
c                              CALL GSPLCI(1)
c                              CALL GSTXCI(1)
c                           ELSE
c                              CALL LINE(XC,YC,U1,V1)
c                           END IF
c                        ELSE
c                           CALL LINE(XC,YC,U1,V1)
c------------------------END IF
                        
                        IF(CACT(I) .GT. CONCMIN)CALL LINE(XC,YC,U1,V1)

                     END IF
                  END IF
               END IF
               XP=XC
               YP=YC
               IF(MOD(ITACT,60).EQ.0)THEN
                  WRITE(LABT,17)IHR,IMN
 17               FORMAT('--',I2,I2.2)

c-----------------Special version for 1998 Cloud Physics Conference
c                 replace colored lines with foreground
c-----------------IF(IGRPLT.EQ.0)THEN
c                    CALL SFLUSH
c                    CALL GSPLCI(IMAGENTA)
c                    CALL GSTXCI(IMAGENTA)
c                    CALL PLCHHQ(XP,YP,LABT,12.0,0.0,-1.0)
c                    CALL SFLUSH
c                    CALL GSPLCI(1)
c                    CALL GSTXCI(1)
c                 ELSE
c                    CALL PLCHHQ(XP,YP,LABT,12.0,0.0,-1.0)
c-----------------END IF

c---ljm--leave out for balloon
c                  CALL PLCHHQ(XP,YP,LABT,12.0,0.0,-1.0)

               END IF
            END IF
            
C           PLACE TIME MARKS ALONG TRACK FOR BOTH RHI AND PPI
C              (1) FIRST POINT INSIDE WINDOW - B
C              (2) MIDDLE TIME - M
C              (3) EVERY 60 SEC - X (magenta)
C              (4) EVERY MJRPT SEC - CIRCLED + SIGN
C     
            IF(FIRST_PT)THEN
c               CALL PLCHMQ (XP,YP,'B',15.0,0.0,0.0)
               CALL PLCHMQ (XP,YP,'T',15.0,0.0,0.0)
               HRC=HRNG
               AZC=AZ
               HEAD=HACT(I)
               ALT=ZACT(I)
               FIRST_PT=.FALSE.
            END IF

c-----------Special version for 1998 Cloud Physics Conference
c           disable plotting of big M at midtime along projection
c-----------IF(TC.EQ.IMSEC)CALL PLCHMQ (XP,YP,'M',12.0,0.0,0.0)
c           IF(MOD(ITACT,60).EQ.0)THEN
c              IF(IGRPLT.EQ.0)THEN
c                 CALL SFLUSH
c                 CALL GSPLCI(IMAGENTA)
c                 CALL GSTXCI(IMAGENTA)
c                 CALL PLCHHQ(XP,YP,'X',12.0,0.0,0.0)
c                 CALL SFLUSH
c                 CALL GSPLCI(1)
c                 CALL GSTXCI(1)
c              ELSE
c                 CALL PLCHHQ(XP,YP,'X',12.0,0.0,0.0)
c              END IF
c-----------END IF

c           IF(MOD(ITACT,MJRPT).EQ.0)THEN
c             CALL PLCHHQ(XP,YP,LAB6,10.0,0.0,0.0)
c           END IF
            NPLOTTED=NPLOTTED+1

 20      CONTINUE

      END IF

      IF(NPLOTTED.EQ.0)RETURN

C     Restore text/line and color back width=ILW and color=foreground (1).
C     Finish up labels for aircraft time interval, location, and heading.
C
      CALL SFLUSH
      CALL GSPLCI(1)
      CALL GSTXCI(1)
      CALL SETUSV('LW',ILW)

      IBSEC=NINT(SECMN)
      IESEC=NINT(SECMX)
      IHRB= IBSEC/3600.0
      IMNB= (IBSEC-3600.0*IHRB)/60.0
      ISCB= IBSEC-3600.0*IHRB-60.0*IMNB
      IHRE= IESEC/3600.0
      IMNE= (IESEC-3600.0*IHRE)/60.0
      ISCE= IESEC-3600.0*IHRE-60.0*IMNE
      WRITE(LABAC,23)IHRB,IMNB,ISCB,IHRM,IMNM,ISCM,
     +     IHRE,IMNE,ISCE,AZC,HRC,ALT,HEAD
 23   FORMAT(3I2.2,'-',3I2.2,'-',3I2.2,' [',F5.1,'dg/',F4.1,'km/',
     +     F4.1,'km/H=',F5.1,'dg]')
      CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,0)
      IF(ITPOLD.EQ.3.AND.NWIN.LE.1)THEN
         XP=FL+0.025
         YP=FT-0.030
         CSIZ=10.0
         CALL TEXTBOX(XP,YP,LABAC,CSIZ)
      ELSE
c         XP=FL
c         YP=FB-0.040
         XP=FX
         YP=FY
         CALL PLCHMQ (XP,YP,LABAC,10.0,0.0,-1.0)
      END IF
      CALL SET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
         
C  PLOT TIME SERIES PANEL OF VERTICAL MOTION
C
      IF(JACT.EQ.2.OR.JACT.EQ.3)THEN
         CALL PLT_TS(TACT,WACT,MXL,IACT,IMSEC,SECMN,SECMX,DTAC,
     X        WMN,WMX,IGRPLT,BGFLAG,TS_LL,TS_SIZ,WTYM,IBLACK,IWHITE)
      END IF

      RETURN
      END
