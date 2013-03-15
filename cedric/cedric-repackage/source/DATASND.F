c
c----------------------------------------------------------------------X
c
      SUBROUTINE DATASND(KRD,LATLON,IBUF,OBUF,RBUF,SBUF,MAXBSZ,IPR)
C
C     This subroutine reads in the locations of mesonet stations 
C     (or other points of interest with a fixed position). This
C     positional information can be used for extracting sounding
C     information for plotting as SkewT diagrams.  Also, soundings
C     can be extracted at regular grid points specified by the user.
C
C     DATSN command line (P2-10):
C       (1) IUNIT: Fortran unit number of the INPUT file
C       (2) CDIR: Direction convention for longitudes
C                 (WEST) - west (east) longitudes are positive (negative)
C                 (EAST) - east (west) longitudes are positive (negative)
C       (3) OLAT: Latitude  of the origin used to convert lat-lon to xy
C       (4) OLON: Longitude of the origin used to convert lat-lon to xy
C       (5) COORD: Specifies whether network positions are in LAT-LONG or XY
C       (6) LATLON: (.FALSE.) Internal plotting grid is XY
C                   (.TRUE.)  Internal plotting grid is LAT-LONG
C
C           LATLON  JLAT COORD ILAT Input --> Conversion --> Output
C             .FALSE. 0    XY      0  XY    --> NONE  (xy) --> XMRK, YMRK
C             .FALSE. 0    LAT-LON 1  LL    --> LL2XY (xy) --> XMRK, YMRK
C             .TRUE.  1    XY      0  XY    --> XY2LL (ll) --> XMRK, YMRK
C             .TRUE.  1    LAT-LON 1  LL    --> NONE  (ll) --> XMRK, YMRK
C       (7) Filter: Winds are 1-D filtered in the vertical direction
C                   using UNIF,TRIA,CRES,QUAD,or EXPO weights.
C       (8) Radius: Number of points to either side of central value.
C       (9) Unused
C      (10) Locations: REGULAR - user-specified regular grid, NET - use
C                      mesonet station locations, or BOTH - regular + net
C           The second line within the DATSND command is for user-specified 
C           regularly spaced grid point locations when P10=REGULAR.
C              Parameters 2-4: Xmin, Xmax, Xskp
C              Parameters 5-7: Ymin, Ymax, Yskp
C     Third line within DATSN command is the list of field names required
C     for the sounding.  The field names must be in the following order:
C     pressure, temperature, dew point, u-wind, v-wind, geopotential height,
C     and topographic height.  Geopotential height will be encoded in dm on 
C     SkewT plots.
C     DATSND must have an 'END' line to terminate the command.
C
C          IBUF- SCRATCH BUFFER F0R I/O
C          OBUF- AUXILLIARY BUFFER FOR DATA MANIPULATION
C          RBUF- I/O DATA BUFFER
C        MAXPLN- MAXIMUM DIMENSION OF RBUF,OBUF
C           IPR- PRINT FILE UNIT NUMBER
C
      INCLUDE 'CEDRIC.INC'
      INCLUDE 'colors.inc'
      CHARACTER*1 BGFLAG,PLTSKEWT

C     HODOGRAPH PLOT:
C        SDFILT - Filter type ('UNIF', 'TRIA')
C        RTFILT - Filter radius (central +/- RTFILT points)
      
      CHARACTER*8 SDFILT,SDFILT_INP
      DATA SDFILT,RTFILT/'NONE',1.0/

      DIMENSION IBUF(MAXPLN),OBUF(MAXPLN),RBUF(MAXBSZ),SBUF(MAXBSZ)
      DIMENSION NAX(3)
      EQUIVALENCE (NCX(1),NX),(NCX(2),NY),(NCX(3),NZ)
      EQUIVALENCE (NAX(1),I1),(NAX(2),I2),(NAX(3),I3)

C     Current edit file characteristics:
C        NAMF      - Fields present
C        CSP       - Current grid specification
C        NCX       - Number of X,Y, and Z grid points
C 
c      PARAMETER (NFMAX=25,NID=510)
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X     IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X     NCXORD(3),NFL,NPLANE,BAD
      COMMON /UNITS/ LIN,LOUT,LPR,LSPOOL
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      DATA SF/100./        
      CHARACTER*2 NAMF,NAMINF(4)
      CHARACTER*8 NAMFLD(NFMAX)

C     Station information:
C        XorLAT - X-coordinate or  Latitude of the station
C        YorLON - Y-coordinate or Longitude of the station
C        XSTA,YSTA,ZSTA - Station locations (lat/lon/z or cartesian)
C        ISTA,JSTA      - (I,J) indices associated with (XSTA,YSTA)
C        IMRK - Number of stations
C        NMRK - Names of stations
C
      PARAMETER(MXK=1000,MXNET=20)
      DIMENSION XSTA(MXK),YSTA(MXK),ZSTA(MXK),NNET(MXNET)
      DIMENSION ISTA(MXK),JSTA(MXK)
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

C     Regular grid informaiton:
C        XREG,YREG - Regular grid locations
C        IREG,JREG - (I,J) indices associated with (XREG,YREG)
C        NREG      - Number of regular grid locations
C        NMREG     - Names of regular grid locations (=REGULAR)

      DIMENSION XREG(MXK),YREG(MXK),IREG(MXK),JREG(MXK)
      CHARACTER*7 NMREG(MXK)

C     SOUNDING INFORMATION:
C        NAMSND           - User-specified field names for sounding
C        (XSND,YSND)      - (X,Y) position (km)
C        (ISND,JSND)      - (I,J) indices associated with (XSND,YSND)
C        NMSND            - Names for regular grid or mesonet locations
C        (PSND,TSND)      - Pressure and air temperature
C        (GSND,ZSND)      - Geopotential heights, topographic heights
C        (DPSND,RHSND)    - Dew point temperature and relative humidity
C        (USND,VSND)      - Horizontal Winds (m/s)
C        (SSND,DSND)      - Wind speed (knots) and direction (deg)
C        MSND             - Number of sounding levels (MSND.le.MXSND)
C
C     Input cloud base conditions and calculate adiabatic values:
C        BASEP, BASET - Cloud base p (mb), T(deg C)
C        AOS    - equivalent potential temperature (saturated adiabat)
C        AOD    - potential temperature (dry adiabat)
C        TCLD   - temperature along saturated (dry) adiabat through 
C                 cld base to represent in-cloud (sub-cloud) temperature
C        DTEMP  - temperature difference (tcloud - tair)
C        CLDMIX - Adiabatic cloud-water mixing ratio (g/kg)
C        CLDLWC -     "     cloud-water content
C
      PARAMETER (MXSND=50)
      REAL MIXBASE
      INTEGER WINSKIP
      CHARACTER*80 TITLE,LABEL
      CHARACTER*8 NMSND(MXK)
      DIMENSION XSND(MXK),YSND(MXK),ISND(MXK),JSND(MXK),
     X     PINP(MXK,MXSND),TINP(MXK,MXSND),GINP(MXK,MXSND),
     X	   DPINP(MXK,MXSND),UINP(MXK,MXSND),VINP(MXK,MXSND),
     X     SINP(MXK,MXSND),DINP(MXK,MXSND),ZINP(MXK,MXSND)
      DIMENSION 
     X     PSND(MXSND),TSND(MXSND),GSND(MXSND),
     X	   DPSND(MXSND),USND(MXSND),VSND(MXSND),
     X     SSND(MXSND),DSND(MXSND),ZSND(MXSND)

      CHARACTER*8 BLANK,NAMSND(7),SNDNAMES(7)
      DATA SNDNAMES
     X     /'PRES(mb)','TEMP (C)','DEWPT(C)','  U-WIND','  V-WIND',
     X      'GHGHT(m)','ZHGHT(m)'/

      CHARACTER*8 GRIDS
      DATA BLANK/'        '/
      DATA TORAD,TODEG,ABSC/0.017453293,57.29577951,273.16/

      BADVAL=BAD
      READ(KRD,3)IUNIT,CDIR,OLAT,OLON,COORD,SDFILT_INP,RTFILT_INP,GRIDS
 3    FORMAT(/I2/A8/F8.0/F8.0/A8/A8/F8.0//A8)
      print *,'DATASND: krd=',iunit,cdir,olat,olon,coord,sdfilt_inp,
     +     rtfilt_inp,grids
      IF(SDFILT_INP.NE.BLANK)SDFILT=SDFILT_INP
      IF(RTFILT_INP.NE.0.0)RTFILT=RTFILT_INP
      
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
C     The following statements were added to correctly open a file
C     in HP UNIX.  SMF Nov 2000.  The old open is commented out.
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

C     SUMMARIZE COMMAND
C
      WRITE(*,5)IUNIT,OLAT,OLON,COORDDES(ILAT+1),PLOTDES(JLAT+1),
     X     CONVERSION
 5    FORMAT(/,5X,'STATION INPUT PARAMETERS...'/,
     X         5X,'                                UNIT:',I4,/
     X         5X,'                                OLAT:',F9.4,/
     X         5X,'                                OLON:',F9.4,/
     X         5X,'      STATION POSITIONS SPECIFIED AS:',2X,A14,/
     X         5X,'          PLOTTING GRID SPECIFIED AS:',2X,A14,/
     X         5X,'                          CONVERSION:',2x,A20/)

C     Current edit file grid specifications:
C     Normal cartesian:   (X,Y) represent distances in km
C     Longitude-latitude: (X,Y) represent degrees
C        Coord  Min Max Spacing Nmb
C          X     X1  X2    XD    N1
C          Y     Y1  Y2    YD    N2
C          Z     Z1  Z2    ZD    N3
C
      X1=CSP(1,1)
      X2=CSP(2,1)
      XD=CSP(3,1)
      Y1=CSP(1,2)
      Y2=CSP(2,2)
      YD=CSP(3,2)
      Z1=CSP(1,3)
      Z2=CSP(2,3)
      ZD=CSP(3,3)

      N1=NCX(1)
      N2=NCX(2)
      N3=NCX(3)
      NPLIN=N1*N2
c-----print *,'X=',x1,x2,xd,n1
c-----print *,'Y=',y1,y2,yd,n2
c-----print *,'Z=',z1,z2,zd,n3

      IMRK=0
      NET=0
 6    READ(IUNIT,7,END=20)(MRKDAT(I),I=1,6)
 7    FORMAT(6A11)
c-----print *,'mrkdat=',mrkdat
c-----print *,'mrkdat(1)(2:2)=',mrkdat(1)(2:2)
      IF(MRKDAT(1)(2:2).EQ.'&')THEN
         NET=NET+1
         IF(NET.GT.20)NET=20
         READ(MRKDAT,8)SMRK(NET),FMRK
 8       FORMAT(1X,A6,4X/F11.0////)
         NNET(NET)=INT(FMRK)
         GO TO 6
      END IF
      IF(MRKDAT(1)(2:8).NE.'STATION')GO TO 6
 10   READ(IUNIT,11,END=20)NSTAT,XorLAT,YorLON,ZMSL
 11   FORMAT(1X,A8,2X,3F11.6)
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

C     Check if the station is located inside the current grid
C     If inside store; otherwise, ignore it.
C
      IDEX=NINT(1.0+(XMRK-X1)/XD)
      JDEX=NINT(1.0+(YMRK-Y1)/YD)
      ZMRK=ZMSL/1000.0

      IF((IDEX .GE. 1 .AND. IDEX .LE. N1) .AND.
     X   (JDEX .GE. 1 .AND. JDEX .LE. N2))THEN

C        Station is inside the current grid
C
         IMRK=IMRK+1
         IF(IMRK.GT.MXK)THEN
            IMRK=IMRK-1
            CLOSE(IUNIT)
            RETURN
         END IF
c         XSTA(IMRK)=XMRK
c         YSTA(IMRK)=YMRK
         XSTA(IMRK)=X1+(IDEX-1)*XD
         YSTA(IMRK)=Y1+(JDEX-1)*YD
         ZSTA(IMRK)=ZMRK
         ISTA(IMRK)=IDEX
         JSTA(IMRK)=JDEX
         WRITE(NMRK(IMRK),13)NSTAT
 13      FORMAT(A7)
         WRITE(*,15)NSTAT,TITL_INP,XorLAT,YorLON,
     X        TITL_OUT,XMRK,YMRK,ZMRK
         WRITE(*,15)NSTAT,TITL_INP,XorLAT,YorLON,
     X        TITL_OUT,XSTA(IMRK),YSTA(IMRK),ZSTA(IMRK)
 15      FORMAT(2X,' In-Name=',2A8,2F12.4,2X,A8,2F12.4,4X,'Z(km)=',F8.3)
         
      ELSE
C        Station is outside the current grid
C
         WRITE(*,17)NSTAT,TITL_INP,XorLAT,YorLON,
     X        TITL_OUT,XMRK,YMRK,ZMRK
 17      FORMAT(2X,'Out-Name=',2A8,2F12.4,2X,A8,2F12.4,4X,'Z(km)=',F8.3)
      END IF
      GO TO 10

C     End reading input station information
C
 20   CONTINUE
      CLOSE(IUNIT)
      IF(GRIDS .NE. 'REGULAR' .AND.
     X   GRIDS .NE. 'NET'     .AND.
     X   GRIDS .NE. 'BOTH')THEN
         PRINT *,
     X'++++ ERROR - Sounding locations have not been specified ++++'
         PRINT *,
     X'++++         P10 must be REGULAR, NET, or BOTH          ++++'
      END IF

C     Read in a regular grid where soundings are to be taken
C
      CALL KARDIN(KRD)
      CALL COMCHK(IPR,KRD)
      READ(KRD,30)XMIN,XMAX,XSKIP,YMIN,YMAX,YSKIP
 30   FORMAT(/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0)
      
C     Make sure user-specified regular grid
C     is compatible with the existing grid.
C
      IF((XMIN-XMAX).GT.0.0 .OR.
     X   (YMIN-YMAX).GT.0.0)THEN
         PRINT *,
     X'++++ ERROR - poorly defined regular grid ++++'
         STOP
      END IF

      IF(XSKIP.LT.1.0)XSKIP=1.0
      IF(YSKIP.LT.1.0)YSKIP=1.0
      ISKIP = NINT(XSKIP)
      JSKIP = NINT(YSKIP)

      IF(XMIN.LT.X1)XMIN=X1
      IF(XMAX.GT.X2)XMAX=X2
      IF(YMIN.LT.Y1)YMIN=Y1
      IF(YMAX.GT.Y2)YMAX=Y2

      IMIN=NINT(1.0+(XMIN-X1)/XD)
      IMAX=NINT(1.0+(XMAX-X1)/XD)
      IF(IMIN.LT. 1)IMIN=1
      IF(IMAX.GT.N1)IMAX=N1
      XMIN=X1+(IMIN-1)*XD
      XMAX=X1+(IMAX-1)*XD
      NIREG=1+(IMAX-IMIN)

      JMIN=NINT(1.0+(YMIN-Y1)/YD)
      JMAX=NINT(1.0+(YMAX-Y1)/YD)
      IF(JMIN.LT. 1)JMIN=1
      IF(JMAX.GT.N2)JMAX=N2
      YMIN=Y1+(JMIN-1)*YD
      YMAX=Y1+(JMAX-1)*YD
      NJREG=1+(JMAX-JMIN)

      WRITE(*,33)XMIN,XMAX,XSKIP,YMIN,YMAX,YSKIP,
     X           IMIN,IMAX,NIREG,JMIN,JMAX,NJREG
 33   FORMAT(/,2X,'SOUNDING GRID...'/,
     X         2X,'  XMIN,XMAX,XSKIP:',3F8.3,/
     X         2X,'  YMIN,YMAX,YSKIP:',3F8.3,/
     X         2X,'  IMIN,IMAX,NIREG:',3I8,/
     X         2X,'  JMIN,JMAX,NJREG:',3I8/)

C     Assign locations and indices from the original
C     grid to the user-specified sounding grid.
C
      NREG=0
      DO J=JMIN,JMAX,JSKIP
         Y=Y1+(J-1)*YD
         DO I=IMIN,IMAX,ISKIP
            X=X1+(I-1)*XD
            NREG=NREG+1
            XREG(NREG)=X
            YREG(NREG)=Y
            IREG(NREG)=I
            JREG(NREG)=J
            NMREG(NREG)='REGULAR'
            print *,'NREG: i,j,x,y=',i,j,x,y
         END DO
      END DO
      print *,'Regular: iskip,jskip,nreg=',iskip,jskip,nreg

C     Combine regular and station locations
C
      print *,'GRIDS=',grids
      IF(GRIDS.EQ.'REGULAR' .OR. GRIDS.EQ.'BOTH')THEN
         DO N=1,NREG
            XSND(N)=XREG(N)
            YSND(N)=YREG(N)
            ISND(N)=IREG(N)
            JSND(N)=JREG(N)
            NMSND(N)='REGULAR'
         END DO
         NTOT=NREG
      END IF
      IF(GRIDS.EQ.'BOTH')THEN
         NBEG=NREG+1
         NEND=NBEG+IMRK
         NN=0
         DO N=NBEG,NEND
            NN=NN+1
            XSND(N)=XSTA(NN)
            YSND(N)=YSTA(NN)
            ISND(N)=ISTA(NN)
            JSND(N)=JSTA(NN)
            NMSND(N)=NMRK(NN)
         END DO
         NTOT=NREG+IMRK
      END IF
      IF(GRIDS.EQ.'NET')THEN
         DO N=1,IMRK
            XSND(N)=XSTA(N)
            YSND(N)=YSTA(N)
            ISND(N)=ISTA(N)
            JSND(N)=JSTA(N)
            NMSND(N)=NMRK(N)
         END DO
         NTOT=IMRK
      END IF

C     Current edit file grid specification.
C     Note: Calculate kilometers from meters for Z-axis
C
      CKM=1.0
      SF = 1./ID(68)
      WRITE(*,35)
 35   FORMAT(/'  CARTESIAN COORDINATE SYSTEM SPECIFICATIONS...'
     X/3X,'AXIS',11X,'MINIMUM    ',5X,'MAXIMUM     ',5X,'DELTA ',
     X7X,'NO. OF PTS.')
      K=160
      DO I=1,3
C        MARK BRADFORD PATCH TO ACCOUNT FOR ID(68).NE.100
         IF(I.EQ.3) CKM=FLOAT(ID(68))/1000.
         R1=ID(K)*SF*CKM
         R2=ID(K+1)*SF*CKM
         R3=ID(K+3)*0.001
         WRITE(*,37) AXNAM(I),R1,LABAXS(I,1),R2,LABAXS(I,1),
     X                 R3,LABAXS(I,1),ID(K+2)
 37      FORMAT(5X,A1,6X,F10.3,1X,A3,3X,F10.3,1X,A3,4X,F8.3,1X,A3,3X,I5)
         K=K+5
      END DO
      L1=ID(164)
      L2=ID(169)
      L3=ID(174)
      CF=1./ID(69)
      R1=ID(40)*CF
      XOR=ID(41)*SF
      YOR=ID(42)*SF
      PRINT *,"  "
      PRINT *,"  GENERAL SCALING FACTOR = ",ID(68)
      PRINT *,"    ANGLE SCALING FACTOR = ",ID(69)
      WRITE(*,40) AXNAM(L1),AXNAM(L2),AXNAM(L3),R1,XOR,YOR
 40   FORMAT(/3X,'AXIS ORDER IS   ',3A3,
     X    /3X,'ANGLE OF THE X-AXIS RELATIVE TO NORTH: ',F9.2,4X,'DEG.',
     X    /3X,'(X,Y)  AXIS ARE SPECIFIED RELATIVE TO:  (',
     X                F7.2,',',F7.2,')')

C     Current edit file field names
C
      NFLD=ID(175)
      WRITE(*,43) NFLD
 43   FORMAT(/'  FIELDS PRESENT: ',I2,' ...'
     X       /4X,'NO.',3X,'NAME',7X,'SCALE FACTOR')
      K2=175
      DO I=1,NFLD
         K1=K2+1
         K2=K2+5
         WRITE(NAMFLD(I),45)(NAMF(J,I),J=1,4)
 45      FORMAT(4A2)
         WRITE(*,47) I,ID(K1),ID(K1+1),ID(K1+2),ID(K1+3),ID(K1+4),
     +        namfld(i)
 47      FORMAT(4X,I3,3X,4A2,5X,I5,' Namfld=',A8)
      END DO

C     Read edit file field names for pressure, temperature, dew point, 
C     u-wind, v-wind, geopotential heights, and topographic heights.
C
      PRINT *," "
      PRINT *,"DATSND: fields to be read for soundings"
      CALL KARDIN(KRD)
      CALL COMCHK(IPR,KRD)
      NINP=7
      DO N=1,NINP
         READ(KRD(N+1),50)NAMSND(N)
 50      FORMAT(A8)
         IF(NAMSND(N).NE.BLANK)THEN
            print *,'        DATSND: ',sndnames(n),'=',namsnd(n)
         END IF
      END DO
      CALL KARDIN(KRD)    

C     Compute equivalent potential temperature (saturated adiabat)
C     and potential temperature (dry adiabat) through cloud base 
C     temperature (BASET, deg C) and pressure (BASEP, mb).
C     Compute mixing ratio (MIXBASE g/kg) at cloud base
C
      BASEP=700.3
      BASET=11.2
      BASET=BASET+ABSC
c      BASEP=BASEP*1000.0
      AOS=OS(BASET,BASEP)
      AOD=OD(BASET,BASEP)
c      TAIR=TDRY(AOD,BASEP) - ABSC
      MIXBASE=W(BASET,BASEP)
      BASET=BASET - ABSC

      WRITE(6,61)BASEP,BASET,AOS,MIXBASE,AOD
 61   FORMAT(1X,'CLOUD BASE CONDITIONS: P=',F6.1,' T=',F6.1,
     +     ' ThetaE=',F6.1, ' Mixing ratio=',F6.1,' Theta=',F6.1)
c      MIXBASE=BADVAL

C     Fetch each of the user-specified field (names = namsnd)
C     for each of N3 vertical levels.
C
      N1=NCX(1)
      N2=NCX(2)
      N3=NCX(3)
      NPLIN=N1*N2
      IFIXAX=3

      DO LEV=1,N3
         DO N=1,NINP
            READ(NAMSND(N),65)NAMINF
 65         FORMAT(4A2)
            IFLD=LOCFLDID(NAMINF,ID(176),5,NFLD,4)
c-----------print *,'Fetch naminf,ifld=',naminf,ifld
            IF(IFLD.EQ.0) THEN
               CALL CEDERX(501,1)
               RETURN
            END IF
c            print *,'Before Fetchd n1,n2,nplin=',n1,n2,nplin
c            print *,'Before Fetchd n3,ifld,bad=',n3,ifld,badval
            
C     FETCH DATA FOR A SINGLE FIELD FROM A PLANE
C        IN     - INPUT UNIT  (IGNORED)
C        ID     - FILE ID HEADER
C        NID    - DIMENSION OF ID
C        LEV    - LEVEL (PLANE) NUMBER
C        IFLD   - FIELD NUMBER
C        IBUF   - SCRATCH BUFFER
C        RBUF   - CONTENTS OF FIELD FOR THIS PLANE
C        N1,N2  - 1st,2nd DIMENSION OF RBUF (RETURNED BY FETCHD)
C        IFIXAX - FIXED AXIS (1,2 OR 3)
C        BAD    - BAD DATA FLAG IN RBUF
C        NST    - STATUS OF OPERATION (0-OK, 1-ERROR)
C     
            CALL FETCHD(IEDFL,ID,LEV,IFLD,IBUF,RBUF,
     X           N1,N2,IFIXAX,BAD,ZLEV,NST)
            nplin=n1*n2
c            print *,'After Fetchd n1,n2,nplin=',n1,n2,nplin
c            print *,'After Fetchd n3,ifld,bad=',n3,ifld,bad
            
            FMIN=9999.
            FMAX=-9999.
            CNT=0.0
            SUM=0.0
            IF(NST.EQ.0)THEN
               DO L=1,NTOT
                  X=XSND(L)
                  Y=YSND(L)
                  I=ISND(L)
                  J=JSND(L)
                  K=I+(J-1)*N1
                  IF(RBUF(K).NE.BAD)THEN
                     IF(RBUF(K).LE.FMIN)FMIN=RBUF(K)
                     IF(RBUF(K).GE.FMAX)FMAX=RBUF(K)
                     SUM=SUM+RBUF(K)
                     CNT=CNT+1.0
c--------------------print *,'j,i,k=',j,i,k,rbuf(k)
                  END IF
                  IF(N.EQ.1)PINP(L,LEV)=RBUF(K)
                  IF(N.EQ.2)TINP(L,LEV)=RBUF(K)
                  IF(N.EQ.3)DPINP(L,LEV)=RBUF(K)
                  IF(N.EQ.4)THEN
                     UINP(L,LEV)=RBUF(K)
                  END IF
                  IF(N.EQ.5)THEN
                     VINP(L,LEV)=RBUF(K)
                     U=UINP(L,LEV)
                     V=VINP(L,LEV)
                     DIR=0.0
                     SPD=0.0
                     IF(U.NE.BAD .AND. V.NE.BAD)THEN
                        SPD=SQRT(U*U+V*V)
                        IF(V.EQ.0.0.AND.U.EQ.0.0)THEN
                           DIR=180.0
                        ELSE IF(V.EQ.0.0.AND.U.GT.0.0)THEN
                           DIR=90.0
                        ELSE IF(V.EQ.0.0.AND.U.LT.0.0)THEN
                           DIR=270.0
                        ELSE
                           DIR=TODEG*ATAN2(U,V)
                           IF(DIR.LT.0.0)DIR=DIR+360.0
                        END IF
                        DIR=DIR+180.0
                        IF(DIR.GE.360.0)DIR=DIR-360.0
                     END IF
                     SINP(L,LEV)=SPD
                     DINP(L,LEV)=DIR
                  END IF
                  IF(N.EQ.6)GINP(L,LEV)=RBUF(K)
                  IF(N.EQ.7)ZINP(L,LEV)=RBUF(K)
               END DO
            END IF
            if(cnt.gt.0.0)avg=sum/cnt
            write(*,1770)namsnd(n),fmin,avg,fmax,int(cnt)
 1770       format('RBUF: name,min,avg,max,cnt=',a8,3f8.1,i8)
         END DO
      END DO

C     CALL DRV_SKEWT to initiate SkewT plot
c        psnd  - pressure (mb)
c        tsnd  - temperature (C)
c        gsnd  - geopotential heights (km)
c        zsnd  - topographic heights (km)
c        dpsnd - dew point temperature (C)
c        ssnd  - wind speed (m/s)
c        dsnd  - wind direction (degrees)
c        mxsnd - maximum number of sounding levels
c        msnd  - actual number of sounding levels
c        title - title for sounding plot
c        aos   - equivalent potential temperature (saturated adiabat)
c        aod   - potential temperature (dry adiabat)
C        basep, baset - Cloud base p (mb), T(deg C)
c        mixbase - cloud base mixing ratio
C     Routine WNDBARB (in skewt) converts (U,V) to SPD (knots)
c     Note: wind barbs have the following interpretation
c        full pennant 50 kts
c        full barb    10 kts
c        half barb     5 kts
c
C     To get current values (IERR=0, no error; ICIR=value of index)
C        GQPLCI(IERR,ICIR) - polyline
C        GQPMCI(IERR,ICIR) - polymarker
C        GQTXCI(IERR,ICIR) - text
C        GQFACI(IERR,ICIR) - fill area
C        GETUSV('II',IV)   - get current value (IV) 
C
C     To overlay hodograph insert, it must be treated as
C     a color plot with white background, black foreground.
C     The insert box is 'colored' with background color
C     before drawing the hodograph.
C     
C     Make background (0) white: CALL GSCR(1,0,1.,1.,1.)
C          foreground (1) black: CALL GSCR(1,1,0.,0.,0.)
C
c      CALL SFLUSH
c      CALL GSCR(1,0,1.,1.,1.)
c      CALL GSCR(1,1,0.,0.,0.)
      ICOLR=0

c     Encode the date, time, station name, and location
c
c     NMRK - name of the station at XSTA,YSTA
c     Model - name of the regular grid
c
      MSND=N3
      WINSKIP=1
      PLTSKEWT='Y'
      UMIN=-25.0
      UMAX=+25.0
      VMIN=-25.0
      VMAX=+25.0
      MIXBASE=BAD

      DO N=1,NTOT
         IHGHT=NINT(100.0*ZINP(N,1))
         WRITE(TITLE,70)NMSND(N),YSND(N),XSND(N),IHGHT
 70      FORMAT(4x,a8,' [Grid Lat=',F7.2,'; Lon=',F7.2,
     X        '; Hght=',I4,' dm]')
         write(*,1771)title
 1771    format('Sounding: ',a80)
         write(*,1772)
 1772    format('  Pres(mb)   Temp(C)  DewPt(C)  Spd(m/s)  Dir(deg)',
     +          ' Ghght(dm) Zhght(dm)')
         DO LEV=1,N3
            GSND(LEV) = 100.0*GINP(N,LEV)
            ZSND(LEV) = 100.0*ZINP(N,LEV)
c            IF(GSND(LEV).GE.ZSND(LEV))THEN
               PSND(LEV) = PINP(N,LEV)
               TSND(LEV) = TINP(N,LEV)
               DPSND(LEV)= DPINP(N,LEV)
               USND(LEV) = UINP(N,LEV)
               VSND(LEV) = VINP(N,LEV)
               SSND(LEV) = SINP(N,LEV)
               DSND(LEV) = DINP(N,LEV)
c            ELSE
c               PSND(LEV)=BAD
c               TSND(LEV)=BAD
c               DPSND(LEV)=BAD
c               USND(LEV)=BAD
c               VSND(LEV)=BAD
c               SSND(LEV)=BAD
c               DSND(LEV)=BAD
c            END IF
            write(*,1773)psnd(lev),tsnd(lev),dpsnd(lev),
     +           ssnd(lev),dsnd(lev),gsnd(lev),zsnd(lev)
 1773       format(7f10.2)
         END DO

         IF(PLTSKEWT.EQ.'Y')THEN
            CALL DRV_SKEW(PSND,TSND,DPSND,SSND,DSND,GSND,MXSND,
     X           MSND,TITLE,AOS,AOD,BASEP,BASET,MIXBASE,WINSKIP,
     X           BAD)

            WRITE(LABEL,100)(ID(I),I=116,121)
 100        FORMAT(I2.2,'/',I2.2,'/',I2.2,'-',I2.2,':',I2.2,':',I2.2)
            CALL MY_PLCHMQ(50,40,LABEL,12.,0.,-1.)

C           Finish with a hodograph overlaid in the upper-left corner
C           SDFILT - User-specified filter weights
C           RTFILT - Filter radius (central +/- RTFILT points)
C
            IF(SDFILT.NE.'NONE')THEN
               IRAD=INT(RTFILT)
               CALL SMTH1D(USND,SDFILT,IRAD,BAD,1,MSND,MSND)
               CALL SMTH1D(VSND,SDFILT,IRAD,BAD,1,MSND,MSND)
            END IF

            CALL PLT_HODO(USND,VSND,GSND,MXSND,MSND,BAD,UMIN,UMAX,
     X           VMIN,VMAX,ICOLR)
            CALL MYFRAME
         END IF
      END DO

C     To set current values (ICLI = color index to be used)
C        GSPLCI(ICLI) - polyline
C        GSPMCI(ICLI) - polymarker
C        GSTXCI(ICLI) - text
C        GSFACI(ICLI) - fill area
C        GACOLR(KAXS,KLBL,KMJT,KMNT)
C                     - sets color indices for axes, labels, major
C                       ticks/grid lines, and minor ticks/grid lines
C                       Default parameter values are zero => no call
C                       is to be made.
C        
      RETURN
      END
c
c----------------------------------------------------------------------x
c
      SUBROUTINE PLT_HODO(U,V,HGHT,MXL,NLEV,BDVAL,UMIN,UMAX,
     X     VMIN,VMAX,ICOLR)
C
C     Plot a hodograph of (U,V) wind values at NLEV heights
C     Input:
C        U,V       - wind components in m/s
C        HGHT      - heights (km or mb)
C        MXL       - maximum dimensions of U,V,HGHT in calling routine
C        NLEV      - actual number of vertical levels to be plotted
C        BDVAL     - Bad (missing) data value
C        UMIN,UMAX - U (X-axis) plotting bounds
C        VMIN,VMAX - V (Y-axis) plotting bounds
C
      CHARACTER IFMTX*6,IFMTY*6,LABB*1
      CHARACTER*8 IXNAM,JXNAM,LABX,LABY
      DATA IXNAM/'U (m/s)'/
      DATA JXNAM/'V (m/s)'/
      LOGICAL FRST

      DIMENSION U(MXL),V(MXL),HGHT(MXL)
      DIMENSION XBOX(5),YBOX(5)

      DATA XRT,YTP,SIDE/0.25,0.98,0.2/
      DATA XOFF,YOFF,ISIZ/0.035,0.035,10/

C     Get parameters of existing plot window
C
      CALL GETSET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)

      X2=XRT
      X1=XRT-SIDE
      Y2=YTP
      Y1=YTP-SIDE

C     COLOR IN A BOX WITH BACKGROUND COLOR BEFORE PROCEEDING
C
      CALL PPI_MAJMIN(UMIN,UMAX,IFMTX,MJX,MNX,IPLX)
      CALL PPI_MAJMIN(VMIN,VMAX,IFMTY,MJY,MNY,IPLY)
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,0)
      BLOFF=(1.35*IPLX*ISIZ)/1024.0
      BROFF=(0.65*IPLY*ISIZ)/1024.0
      BTOFF=BROFF
      BBOFF=(1.55*IPLY*ISIZ)/1024.0

      XBOX(1)=X1-BLOFF
      XBOX(2)=XBOX(1)
      XBOX(3)=X2+BROFF
      XBOX(4)=XBOX(3)
      XBOX(5)=XBOX(1)

      YBOX(1)=Y1-BBOFF
      YBOX(2)=Y2+BTOFF
      YBOX(3)=YBOX(2)
      YBOX(4)=YBOX(1)
      YBOX(5)=YBOX(1)

      CALL GSFACI (ICOLR)
      CALL GFA (5,XBOX,YBOX)
      do i=1,4
         u1=xbox(i)
         u2=xbox(i+1)
         v1=ybox(i)
         v2=ybox(i+1)
         call line(u1,v1,u2,v2)
      end do

C     Plot and label the axes and central reference (U0,V0)
C
      CALL SET (X1,X2,Y1,Y2,UMIN,VMAX,VMIN,VMAX,0)
      CALL LABMOD(IFMTX,IFMTY,IPLX,IPLY,ISIZ,ISIZ,8,8,0)
      CALL TICK4(24,12,24,12)
      CALL GRIDAL(MJX,MNX,MJY,MNY,1,1,5,0,0)

      CALL DASHDB (O'104210')
      U0=0.0
      V0=0.0
      IF((U0.GT.UMIN.AND.U0.LT.UMAX) .AND.
     X   (V0.GT.VMIN.AND.V0.LT.VMAX))THEN
         CALL LINED(UMIN,V0,UMAX,V0)
         CALL LINED(U0,VMIN,U0,VMAX)
      END IF

C     Plot the (U,V) wind hogograph
C
      CALL SET(X1,X2,Y1,Y2,UMIN,UMAX,VMIN,VMAX,1)

      FRST=.TRUE.
      DO 20 L=1,NLEV-1
         IHGHT=NINT(HGHT(L))
         U1=U(L)
         U2=U(L+1)
         V1=V(L)
         V2=V(L+1)
         IF(U1.EQ.BDVAL.OR.V1.EQ.BDVAL)GO TO 20
         IF(U2.EQ.BDVAL.OR.V2.EQ.BDVAL)GO TO 20
         IF(U1.LT.UMIN .OR. U1.GT.UMAX)GO TO 20
         IF(U2.LT.UMIN .OR. U2.GT.UMAX)GO TO 20
         IF(V1.LT.VMIN .OR. V1.GT.VMAX)GO TO 20
         IF(V2.LT.VMIN .OR. V2.GT.VMAX)GO TO 20
         CALL LINE(U1,V1,U2,V2)
         IF(FRST)THEN
            CALL PLCHMQ(U1,V1,'X',8.0,0.,0.0)
            FRST=.FALSE.
         END IF
 20   CONTINUE

      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      
      WRITE(LABX,23)IXNAM
      WRITE(LABY,23)IYNAM
 23   FORMAT(A7)
      
      XP=XRT-0.5*SIDE
      YP=YTP-SIDE-YOFF
      CALL PLCHMQ (XP, YP, LABX, 12.0, 0.0, 0.0)

      XP=XRT-SIDE-XOFF
      YP=YTP-0.5*SIDE
      CALL PLCHMQ (XP, YP, LABY, 12.0, 90.0, 0.0)

C     Restore parameters of original plot window
C
      CALL SET(FL,FR,FB,FT,UL,UR,UB,UT,LLL)
      CALL SFLUSH
      CALL GSPLCI(1)
      CALL GSTXCI(1)
      RETURN
      END
