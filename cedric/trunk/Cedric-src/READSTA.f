      SUBROUTINE READSTA(KRD,XSTA,YSTA,ZSTA,IMRK,NET,NNET,SMRK,NMRK,
     X     LATLON)
C
C     THIS SUBROUTINE READS IN THE LOCATIONS OF MESONET STATIONS 
C     (OR OTHER POINTS OF INTEREST WITH A FIXED POSITION). THE
C     DATA CAN THEN BE PLOTTED WITH THE PLTSTA SUBCOMMAND OF THE
C     GRAPHICS COMMAND. THE ACTUAL PLOTTING IS DONE IN PLTSTA
C     SUBROUTINE.
C
C     IUNIT  - Fortran unit number of the INPUT file
C     CDIR   - Direction convention for longitudes
C              (WEST) - west (east) longitudes are positive (negative)
C              (EAST) - east (west) longitudes are positive (negative)
C     OLAT   - Latitude  of the origin used to convert lat-lon to xy
C     OLON   - Longitude of the origin used to convert lat-lon to xy
C     XorLAT - X-coordinate or  Latitude of the station
C     YorLON - Y-coordinate or Longitude of the station
C     COORD  - Specifies whether network positions are in LAT-LONG or XY
C     LATLON - (.FALSE.) Internal plotting grid is XY
C              (.TRUE.)  Internal plotting grid is LAT-LONG
C
C             LATLON  JLAT COORD ILAT Input --> Conversion --> Output
C             .FALSE. 0    XY      0  XY    --> NONE  (xy) --> XMRK, YMRK
C             .FALSE. 0    LAT-LON 1  LL    --> LL2XY (xy) --> XMRK, YMRK
C             .TRUE.  1    XY      0  XY    --> XY2LL (ll) --> XMRK, YMRK
C             .TRUE.  1    LAT-LON 1  LL    --> NONE  (ll) --> XMRK, YMRK
C

      PARAMETER (NFMAX=25,NID=510)
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X     IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X     NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      PARAMETER(MXK=1000,MXNET=20)
      DIMENSION XSTA(MXK),YSTA(MXK),ZSTA(MXK),NNET(MXNET)
      CHARACTER*6 SMRK(MXNET)
      CHARACTER*7 NMRK(MXK)
      CHARACTER*11 MRKDAT(6)
      CHARACTER*8 KRD(10),CDIR,COORD,NSTAT,TITL_INP,TITL_OUT
      CHARACTER*14 COORDDES(2),PLOTDES(2)
      CHARACTER*20 CONVERSION
      CHARACTER*7 FILENAME
      LOGICAL LATLON
      DATA COORDDES/'X-Y VALUES','LAT-LON VALUES'/
      DATA PLOTDES /'X-Y VALUES','LAT-LON VALUES'/

      READ(KRD,70)IUNIT,CDIR,OLAT,OLON,COORD
 70   FORMAT(/I2/A8/F8.0/F8.0/A8)

      IF (CDIR.EQ.'EAST') OLON=-OLON
      IF (KRD(4).EQ.' ') THEN
         OLAT=ID(33) + ID(34)/60. + (ID(35)/FLOAT(ID(68)))/3600.
      END IF
      IF (KRD(5).EQ.' ') THEN
         OLON=ID(36) + ID(37)/60. + (ID(38)/FLOAT(ID(68)))/3600.
      END IF
      ANGXAX=ID(40)/FLOAT(ID(69))

C     Set flag for network coordinates: (0) XY, (1) LatLon (default)
C
      IF (COORD.EQ.'XY') THEN
         ILAT=0
      ELSE
         ILAT=1
      END IF

C     Set flag for Cedric internal grid: (1) LatLon, (0) XY (default)
C
      IF (LATLON) THEN
         JLAT=1
      ELSE
         JLAT=0
      END IF

      IF(JLAT.EQ.0)THEN
         ISWAP=0
         IF(ILAT.EQ.0)THEN
C           Grid is XY and Stations are XY
C
            TITL_INP='Inp-XY='
            TITL_OUT='Out-XY='
            CONVERSION='NONE'
         ELSE
C           Grid is XY and Stations are LL
C
            TITL_INP='Inp-LL='
            TITL_OUT='Out-XY='
            CONVERSION='LatLon --> XY'
         END IF

      ELSE
         ISWAP=1
         IF(ILAT.EQ.0)THEN
C           Grid is LL and Stations are XY
C
            TITL_INP='Inp-XY='
            TITL_OUT='Out-LL='
            CONVERSION='XY --> LonLat'
         ELSE
C           Grid is LL and Stations are LL
C
            TITL_INP='Inp-LL='
            TITL_OUT='Out-LL='
            CONVERSION='LatLon --> LonLat'
         END IF
      END IF
C
C     NOW START READING FROM DATA FILE THAT CORRESPONDS TO IUNIT
C
CThe following statements were added to open the file correctly
Cin HP unix.  SMF Nov 2000.  The old open is commented out.
C     05/09/2001 - ljm fix error (itwo = iunit - 10)

      FILENAME = "fort."
      if(iunit .LT. 10) then
         FILENAME(6:6) = char(iunit + 48)
      end if

      if(iunit .ge. 10 .and. iunit .lt. 100) then
          ione = iunit/10
          itwo = iunit - 10*ione
          FILENAME(6:6) = char(ione + 48)
          FILENAME(7:7) = char(itwo + 48)
      endif
C      OPEN(UNIT=IUNIT,FORM='FORMATTED',ACCESS='SEQUENTIAL',
C     X     IOSTAT=IOVAL)
       OPEN(UNIT = IUNIT, FILE = FILENAME, IOSTAT = IOVAL)
      IF (IOVAL.NE.0) THEN
         CALL CEDERX(584,1)
         RETURN
      END IF
C
C     SUMMARIZE COMMAND
C
      WRITE(*,100)IUNIT,OLAT,OLON,COORDDES(ILAT+1),PLOTDES(JLAT+1),
     X     CONVERSION
 100  FORMAT(/,5X,'STATION INPUT PARAMETERS...'/,
     X         5X,'                                UNIT:',I4,/
     X         5X,'                                OLAT:',F9.4,/
     X         5X,'                                OLON:',F9.4,/
     X         5X,'      STATION POSITIONS SPECIFIED AS:',2X,A14,/
     X         5X,'          PLOTTING GRID SPECIFIED AS:',2X,A14,/
     X         5X,'                          CONVERSION:',2x,A20/)

      IMRK=0
      NET=0
    6 READ(IUNIT,61,END=20)(MRKDAT(I),I=1,6)
   61 FORMAT(6A11)
c-----print *,'mrkdat=',mrkdat
c-----print *,'mrkdat(1)(2:2)=',mrkdat(1)(2:2)
      IF(MRKDAT(1)(2:2).EQ.'&')THEN
         NET=NET+1
         IF(NET.GT.20)NET=20
         READ(MRKDAT,8)SMRK(NET),FMRK
    8    FORMAT(1X,A6,4X/F11.0////)
         NNET(NET)=INT(FMRK)
         GO TO 6
      END IF
      IF(MRKDAT(1)(2:8).NE.'STATION')GO TO 6
   10 READ(IUNIT,11,END=20)NSTAT,XorLAT,YorLON,ZMSL
   11 FORMAT(1X,A8,2X,3F11.6)
      IF(XorLAT.EQ.0.0 .AND. YorLON.EQ.0.0 .AND. ZMSL.EQ.0.0)GO TO 10

      IF(JLAT.EQ.0)THEN
         IF(ILAT.EQ.0)THEN
C           Grid is XY and Stations are XY
C
            XMRK=XorLAT
            YMRK=YorLON
         ELSE
C           Grid is XY and Stations are LL
C
            IF (CDIR.EQ.'EAST') YorLON=-YorLON
            CALL LL2XYDRV(XorLAT,YorLON,X,Y,OLAT,OLON,ANGXAX)
            XMRK=X
            YMRK=Y
         END IF

      ELSE
         IF(ILAT.EQ.0)THEN
C           Grid is LL and Stations are XY
C
            CALL XY2LLDRV(PLAT,PLON,XorLAT,YorLON,OLAT,OLON,ANGXAX)
            IF (CDIR.EQ.'EAST') PLON=-PLON
            XMRK=PLON
            YMRK=PLAT
         ELSE
C           Grid is LL and Stations are LL
C
            XMRK=YorLON
            YMRK=XorLAT
         END IF
      END IF

      IMRK=IMRK+1
      IF(IMRK.GT.MXK)THEN
         IMRK=IMRK-1
         CLOSE(IUNIT)
         RETURN
      END IF
      XSTA(IMRK)=XMRK
      YSTA(IMRK)=YMRK
      ZSTA(IMRK)=ZMSL/1000.0
      WRITE(NMRK(IMRK),13)NSTAT
   13 FORMAT(A7)
      WRITE(6,1770)NSTAT,TITL_INP,XorLAT,YorLON,
     X     TITL_OUT,XSTA(IMRK),YSTA(IMRK),ZSTA(IMRK)
 1770 FORMAT(2X,'Name=',2A8,2F12.4,2X,A8,2F12.4,4X,'Z(km)=',F8.3)
      GO TO 10
   20 CONTINUE
      CLOSE(IUNIT)
      RETURN
      END



