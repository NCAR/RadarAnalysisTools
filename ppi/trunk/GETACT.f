c
c----------------------------------------------------------------------X
c
      SUBROUTINE GETACT(INDAT,XACT,YACT,ZACT,UACT,VACT,WACT,CACT,QACT,
     X     DACT,HACT,TACT,DTAC,DPAC,IACT,OLAT,OLON,ANGXAX,TMJR,TMNR,
     X     TS_LL,TS_SIZ,VECSCL,SCLR,WTYM,WMN,WMX,MXL,ORLAT,ORLON)
C
C  READ IN AIRCRAFT POSITIONS AND WINDS FROM ACTFILE:
C     CTYP=LL, CONVERT (PLAT,PLON) TO (X,Y) DISTANCE FROM (OLAT,OLON)
C     CTYP=XY, CONVERT (X,Y) DISTANCE TO (PLAT,PLON)   "       "
C  Note: (OLAT,OLON) MUST BE THE (LAT,LON) OF THE PLOTTING WINDOW ORIGIN,
C        WHERE THE WINDOW IS DEFINED BY (GXMIN,GXMAX,GYMIN,GYMAX)
C     FILTER THE TIME SERIES USING 'UNIForm' OR 'TRIAngular' filter
C     WITH RADIUS RTFILT.
C
C     TIME             - INPUT TIME (SEC OR HMS)
C     (XINP,YINP)      - INPUT (X,Y) POSITION, EITHER LAT/LON ('LL') or 
C                        Kilometers ('XY')
C     (XACT,YACT,ZACT) - (X,Y,Z) POSITION (KM), Z in Meters ('M'), 
C                        Decameters ('D'), or Kilometers (' ')
C     (UACT,VACT,WACT) - WINDS (M/S)
C     (CACT,QACT,DACT) - FSSP CONC, LWC, and DBZ (#/cc, g/m3, dBZ)
C     HACT             - AZIMUTH TOWARD WHICH THE AIRCRAFT IS HEADED.
C     (TACT)           - TIME (SEC)
C     (DTAC,DPAC)      - TIME WINDOW (RADAR CENTRAL TIME +/- DTAC SEC)
C                        AND SPACE WINDOW (RADAR SCAN +/- DPAC KM)
C                        Note: Set DTAC to -DTAC prior to plotting PLOTPRJ
C                        in order to plot DTAC seconds outside the Volume
C                        times.  Normally plotting is +/-DTAC from scan
C                        central time.  See PPI_MMM prior to PLTACT call.
C     (TMJR,TMNR)      - INTERVALS (SEC) BETWEEN (MAJOR,MINOR) MARKS
C                      - WIND VECTORS PLOTTED EVERY MINOR SEC
C     WTYM             _ PLOT INTERVAL (SEC) FOR WIND INSERT
C     IACT             - NUMBER OF AIRCRAFT MEASUREMENTS (IACT.LE.MXL)
C     TS_LL            - FRACTIONAL LOWER-LEFT CORNER FOR W INSERT
C     TS_SIZ           - FRACTIONAL XY SIZES FOR W INSERT
C     VECSCL           - WIND VECTOR PLOTTING SCALE (M/S PER KM)
C     SCLR             - SCALAR PLOTTING SCALE (UNITS OF SCALAR PER KM)
C     (WMN,WMX)        - (MIN,MAX) FOR W INSERT PLOT
C     ACFILT           - TIME SERIES FILTER ('UNIF', 'TRIA')
C     RTFILT           - FILTER RADIUS (CENTRAL +/- RTFILT POINTS)
C     
C     Structure of aircraft track input file created by dbz_probes.f
C          TIME           - time (GMT sec)
C          XINP,YINP,ZMSL - (x,y) km and height (m)
C          U,V,W          - (u,v,w) m/s
C          hhmmss         - time (GMT hhmmss)
C          az,hdist       - azimuth and horizontal distance (km)
C          concfsp,lwcfsp,dbzfsp - FSSP (conc, lwc, dbz)
C          lwc26x,dbz26x  - 260X (lwc, dbz)
C
      DIMENSION XACT(MXL),YACT(MXL),ZACT(MXL),TACT(MXL)
      DIMENSION UACT(MXL),VACT(MXL),WACT(MXL)
      DIMENSION CACT(MXL),QACT(MXL),DACT(MXL),HACT(MXL)
      CHARACTER ACTFILE*24,IDIR*4,ACFILT*8,TTYP*3,CTYP*3
      CHARACTER*8 INDAT(10),JNDAT(10)
      CHARACTER*8 TS_LL,TS_SIZ

      INTEGER HHMMSS
      REAL LWCFSP,LWC26X

      DATA TODEG/57.29577951/

      READ(INDAT,1)TTYP,CTYP,IDIR,OLAT,OLON,DTAC,DPAC,TMJR,TMNR
 1    FORMAT(////A3,1X,A3,1X/A4,4X/F8.0/F8.0/F4.0,F4.0/F4.0,F4.0)
      IF(OLAT.EQ.0.0 .AND. ORLAT.NE.0.0)OLAT=ORLAT
      IF(OLON.EQ.0.0 .AND. ORLON.NE.0.0)OLON=ORLON
      IF(IDIR.EQ.'WEST')THEN
         OLON=+1.0*ABS(OLON)
      ELSE IF(IDIR.EQ.'EAST')THEN
         OLON=-1.0*ABS(OLON)
      END IF
      ACTFILE=INDAT(2)//INDAT(3)//INDAT(4)
      WRITE(6,2)ACTFILE,TTYP,CTYP,IDIR,OLAT,OLON,DTAC,DPAC,TMJR,TMNR
 2    FORMAT(1X,'GETACT: AIRCRAFT POSITION FILE= ',A24,A3,1X,A3,1X,
     +       A4,4X,2F12.5,4F8.1)
      OPEN(UNIT=9,FILE=ACTFILE,STATUS='OLD')
      READ(5,3)(JNDAT(I),I=1,10)
 3    FORMAT(10A8)
c      WRITE(6,31)(JNDAT(I),I=1,10)
c 31   FORMAT('Kardin=',10A8)
      IF(JNDAT(2).NE.'WINDS   ')THEN
         WRITE(6,4)
 4       FORMAT('*** GETACT MUST HAVE WINDS LINE, FOLLOWED BY END ***')
         STOP
      END IF
      READ(JNDAT,5)VECSCL,SCLR,WTYM,TS_LL,TS_SIZ,WMN,WMX,ACFILT,RTFILT
 5    FORMAT(//F4.0,F4.0/F8.0/A8/A8/F8.0/F8.0/A4,4X,/F8.0)
      READ(5,3)(JNDAT(I),I=1,10)
c      WRITE(6,31)(JNDAT(I),I=1,10)
      IF(JNDAT(1).NE.'END     ')THEN
         WRITE(6,4)
         STOP
      END IF
      WRITE(6,6)TS_LL(1:4),TS_LL(5:8),TS_SIZ(1:4),TS_SIZ(5:8),VECSCL,
     +     WTYM,WMN,WMX,ACFILT,RTFILT
 6    FORMAT(1X,'GETACT: TS_LL,TS_SIZ,SCL,WT,WMNX,FILT,RAD= ',4(A4,2X),
     +     4F8.2,2X,A4,2X,F8.1)
      ITMNR=NINT(TMNR)
      ITMJR=NINT(TMJR)
      write(*,*)'tmjr,tmnr=',itmjr,itmnr

      IACT=0

c-----ljm changed to read balloon track as an aircraft track
c
c 10   READ(9,*,ERR=10,END=20)TIME,XINP,YINP,ZMSL,U,V,W,
c     +     hhmmss,az,hdist,concfsp,lwcfsp,dbzfsp,lwc26x,dbz26x
 10   READ(9,*,ERR=10,END=20)TIME,XINP,YINP,ZMSL
c      print *,'Time,x,y,z=',TIME,XINP,YINP,ZMSL
c
c-----ljm changed to read balloon track as an aircraft track

      IF(TIME.EQ.0.0 .AND. XINP.EQ.0.0 .AND. YINP.EQ.0.0)GO TO 10

      IF(CTYP(1:2).EQ.'LL')THEN
         PLAT=XINP
         IF(IDIR.EQ.'WEST')THEN
            PLON=+1.0*ABS(YINP)
         ELSE IF(IDIR.EQ.'EAST')THEN
            PLON=-1.0*ABS(YINP)
         END IF
         CALL LL2XYDRV(PLAT,PLON,X,Y,OLAT,OLON,ANGXAX)
      ELSE 
         X=XINP
         Y=YINP
         CALL XY2LLDRV(PLAT,PLON,X,Y,OLAT,OLON,ANGXAX)
      END IF

      IACT=IACT+1
      IF(IACT.GT.MXL)THEN
         IACT=IACT-1
         GO TO 20
      END IF
      XACT(IACT)=X
      YACT(IACT)=Y
      IF(CTYP(3:3).EQ.'M')THEN
         ZACT(IACT)=ZMSL/1000.0
      ELSE IF (CTYP(3:3).EQ.'D')THEN
         ZACT(IACT)=ZMSL/100.0
      ELSE
         ZACT(IACT)=ZMSL
      END IF
c-----ljm changed to read balloon track as an aircraft track
c
c      UACT(IACT)=U
c      VACT(IACT)=V
c      WACT(IACT)=W
c      CACT(IACT)=CONCFSP
c      QACT(IACT)=LWCFSP
c      DACT(IACT)=DBZFSP
c
c-----ljm changed to read balloon track as an aircraft track

C     Calculate horizontal range, azimuth, and elevation angle
C     of aircraft relative to the origin (OLAT,OLON).  Using
C     Goodland radar at z=1122.8 meter for the time being.
C
      XORIGIN=0.0
      YORIGIN=0.0
      ZORIGIN=1.123
      XP=XACT(IACT)-XORIGIN
      YP=YACT(IACT)-YORIGIN
      ZP=ZACT(IACT)-ZORIGIN
      HRNG=SQRT(XP*XP+YP*YP)
      IF(XP.EQ.0.0 .AND. YP.EQ.0.0)THEN
         AZIM=0.0
      ELSE IF(XP.GT.0.0 .AND. YP.EQ.0.0)THEN
         AZIM=90.0
      ELSE IF(XP.EQ.0.0 .AND. YP.LT.0.0)THEN
         AZIM=180.0
      ELSE IF(XP.LT.0.0 .AND. YP.EQ.0.0)THEN
         AZIM=270.0
      ELSE IF(XP.NE.0.0 .AND. YP.NE.0.0)THEN
         AZIM=TODEG*ATAN2(XP,YP)
      ELSE
         AZIM=0.0
      END IF
      IF(AZIM.LT.0.0)AZIM=AZIM+360.0
      IF(HRNG.EQ.0.0.AND.ZP.EQ.0.0)THEN
         ELEV=0.0
      ELSE IF(HRNG.EQ.0.0.AND.ZP.GT.0.0)THEN
         ELEV=90.0
      ELSE IF(HRNG.EQ.0.0.AND.ZP.LT.0.0)THEN
         ELEV=-90.0
      ELSE
         ELEV=TODEG*ATAN2(ZP,HRNG)
      END IF

      IF(TTYP.EQ.'SEC')THEN
         SEC=TIME
         IHR=SEC/3600.0
         IMN=(SEC-IHR*3600.0)/60.0
         ISC= SEC-IHR*3600.0-IMN*60.0
         IF(ISC.GE.60)THEN
            ISC=ISC-60
            IMN=IMN+1
         END IF
         IF(IMN.GE.60)THEN
            IMN=IMN-60
            IHR=IHR+1
         END IF
      ELSE
         IHR=TIME/10000.
         IMN=(TIME-IHR*10000.)/100.
         ISC=TIME-IHR*10000.-IMN*100.
         SEC=IHR*3600.+IMN*60.+FLOAT(ISC)
      END IF
      TACT(IACT)=SEC
      ISEC=INT(TACT(IACT))
      IHHMMSS=IHR*10000+IMN*100+ISC

c     Note: I2.2 causes leading zeros to be printed,
c           e.g. 13:01:05 rather than 13: 1: 5 for HH:MM:SS.
c
      IF(MOD(ISEC,60).EQ.0)THEN
c--------ljm changed to write as a balloon track
c         WRITE(6,13)ISEC,IHR,IMN,ISC,X,Y,ZMSL,AZIM,HRNG,
c     +        U,V,W,CONCFSP,LWCFSP,DBZFSP,LWC26X,DBZ26X
c 13      FORMAT('*T=',I5,1X,3I2.2,'  Loc=',3F8.2,2F6.1,'  Var=',8F8.2)
c      ELSE
c         WRITE(6,15)ISEC,IHR,IMN,ISC,X,Y,ZMSL,AZIM,HRNG,
c     +        U,V,W,CONCFSP,LWCFSP,DBZFSP,LWC26X,DBZ26X
c 15      FORMAT(' T=',I5,1X,3I2.2,'  Loc=',3F8.2,2F6.1,'  Var=',8F8.2)
c--------ljm changed to write as a balloon track
         WRITE(6,13)IHR,IMN,ISC,ISEC,XINP,YINP,X,Y,ZMSL,HRNG,AZIM,ELEV
 13      FORMAT('*T=',I2.2,':',I2.2,':',I2.2,' Sec=',I5,
     +        ' Lat/lon=',2f10.4,'  XYZ=',3F8.2,'  HAE=',3F6.1)
      ELSE
         WRITE(6,15)IHR,IMN,ISC,ISEC,XINP,YINP,X,Y,ZMSL,HRNG,AZIM,ELEV
 15      FORMAT(' T=',I2.2,':',I2.2,':',I2.2,' Sec=',I5,
     +        ' Lat/lon=',2f10.4,'  XYZ=',3F8.2,'  HAE=',3F6.1)
      END IF
      GO TO 10

 20   CONTINUE
      IF(IACT.EQ.0)THEN
         WRITE(6,99)
 99      FORMAT(1X,'NO AIRCRAFT TRACK WAS READ')
         STOP
      END IF

C     Calculate azimuth toward which the aircraft is headed.
C
      DO I=2,IACT-1
         DX=0.5*(XACT(I+1)-XACT(I-1))
         DY=0.5*(YACT(I+1)-YACT(I-1))
         IF(DX.EQ.0.0 .AND. DY.EQ.0.0)THEN
            AZ=0.0
         ELSE IF(DX.GT.0.0 .AND. DY.EQ.0.0)THEN
            AZ=90.0
         ELSE IF(DX.EQ.0.0 .AND. DY.LT.0.0)THEN
            AZ=180.0
         ELSE IF(DX.LT.0.0 .AND. DY.EQ.0.0)THEN
            AZ=270.0
         ELSE IF(DX.NE.0.0 .AND. DY.NE.0.0)THEN
            AZ=TODEG*ATAN2(DX,DY)
         ELSE
            AZ=0.0
         END IF
         IF(AZ.LT.0.0)AZ=AZ+360.0
         HACT(I)=AZ
      END DO
      HACT(1)=HACT(2)
      HACT(IACT)=HACT(IACT-1)

      IF(ACFILT.NE.'NONE')THEN
         IRAD=INT(RTFILT)
         BDVAL=-999.0
         IF(VECSCL.GT.0.0)THEN
            CALL SMTH1D(UACT,ACFILT,IRAD,BDVAL,1,IACT,IACT)
            CALL SMTH1D(VACT,ACFILT,IRAD,BDVAL,1,IACT,IACT)
         END IF
         IF(WTYM.GT.0.0)THEN
            CALL SMTH1D(WACT,ACFILT,IRAD,BDVAL,1,IACT,IACT)
            CALL SMTH1D(CACT,ACFILT,IRAD,BDVAL,1,IACT,IACT)
            CALL SMTH1D(QACT,ACFILT,IRAD,BDVAL,1,IACT,IACT)
            CALL SMTH1D(DACT,ACFILT,IRAD,BDVAL,1,IACT,IACT)
         END IF
      END IF
      RETURN
      END




