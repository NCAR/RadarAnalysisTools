      SUBROUTINE READAIR(KRD,XACT,YACT,ZACT,BEGACT,DELACT,NACT,DTAC,
     X     AIRTHK,UACT,VACT,WACT,IACTWND,RBUF,IBUF,LATLON)
C
C     THIS SUBROUTINE READS IN AIRCRAFT TRACK POSITIONS FROM AN ASCII FILE 
C     FOR USE LATER IN PLOTTING (SEE AIRTRCK AND CONTOUR SUBCOMMANDS OF
C     GRAPHICS COMMAND).
C
C     IUNIT  - Fortran unit number of the INPUT file
C     CDIR   - Direction convention for longitudes
C              (WEST) - west (east) longitudes are positive (negative)
C              (EAST) - east (west) longitudes are positive (negative)
C     OLAT   - Latitude  of the origin used to convert lat-lon to xy
C     OLON   - Longitude of the origin used to convert lat-lon to xy
C     DELACT - Plotting time step (sec) .ge. input track delta time
C     COORD  - Specifies whether network positions are in LAT-LONG or XY
C     DTAC   - Input times +/- sec from central time of Cedric volume
C     AIRTHK - Line thickness multiplier for aircraft track
C              Thickness = AIRTHK * default of 1000
C     LATLON - (.FALSE.) Internal plotting grid is XY
C              (.TRUE.)  Internal plotting grid is LAT-LONG
C
C     LATLON  JLAT COORD ILAT Input --> Conversion --> Output
C     .FALSE.  0    XY      0  XY    --> NONE  (xy) --> XMRK, YMRK
C     .FALSE.  0    LAT-LON 1  LL    --> LL2XY (xy) --> XMRK, YMRK
C     .TRUE.   1    XY      0  XY    --> XY2LL (ll) --> XMRK, YMRK
C     .TRUE.   1    LAT-LON 1  LL    --> NONE  (ll) --> XMRK, YMRK
C
C     XorLAT - X-coordinate or  Latitude of the aircraft
C     YorLON - Y-coordinate or Longitude of the aircraft
C
      INCLUDE 'CEDRIC.INC'
      PARAMETER (MXL=20000)
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X     IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X     NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF
      DIMENSION XACT(MXL),YACT(MXL),ZACT(MXL),UACT(MXL),VACT(MXL)
      DIMENSION WACT(MXL),WIND(10),RBUF(1)
      CHARACTER*8 KRD(10),CDIR,COORD,NAMOUT(3),TITL_INP,TITL_OUT
      CHARACTER*14 COORDDES(2),PLOTDES(2)
      CHARACTER*20 CONVERSION
      CHARACTER*7 FILENAME
      DATA COORDDES/'X-Y VALUES','LAT-LON VALUES'/
      DATA PLOTDES /'X-Y VALUES','LAT-LON VALUES'/
      DATA DTR/0.0174533/
      DATA IU,IV,IW/0,0,0/

      LOGICAL LATLON
C
C     Get Cedric volume times:
C        BSEC, TMV, ESEC - beginning, middle, ending seconds
C        BTIME, ETIME - both are set to the middle time (TMV)
C
      BSEC=ID(119)*3600. + ID(120)*60. + ID(121)
      ESEC=ID(125)*3600. + ID(126)*60. + ID(127)
      PRINT *,'    Beg: hhmmss=',id(119),id(120),id(121),' sec=',bsec
      PRINT *,'    End: hhmmss=',id(125),id(126),id(127),' sec=',esec
      IF (ESEC.LT.BSEC) ESEC = ESEC + 3600.*24
      TMV=(BSEC+ESEC)/2.

      IACTWND=0
      IGRID  =0
      ASPD   =0.0
      DO I=1,3
         NAMOUT(I)=' '
      END DO

      READ(KRD,10)IUNIT,CDIR,OLAT,OLON,DELACT,COORD,DTAC,AIRTHK
 10   FORMAT(/I2/A8/F8.0/F8.0/F8.0/A8/F8.0/F8.0)

C     DTAC .gt. 0: Read DTAC before/after central time (TMV)
C          .le. 0: Read DTAC before/after beg/end times
C
      IF(DTAC.GT.0.0)THEN
         BTIME=TMV-DTAC
         IF (BTIME.LT.0) BTIME = BTIME + 3600.*24
         ETIME=TMV+DTAC
         IF (ETIME.LT.BTIME) ETIME = ETIME + 3600.*24
      ELSE
         BTIME=BSEC-DTAC
         IF (BTIME.LT.0) BTIME = BTIME + 3600.*24         
         ETIME=ESEC+DTAC
         IF (ETIME.LT.BTIME) ETIME = ETIME + 3600.*24
      END IF

      IF (AIRTHK.LE.0.0)AIRTHK=1.0
      IF (DELACT.LE.0.0) DELACT=1.0
      IF (CDIR.EQ.'EAST') OLON=-OLON
      IF (KRD(4).EQ.' ') THEN
         OLAT=ID(33) + ID(34)/60. + (ID(35)/FLOAT(ID(68)))/3600.
      END IF
      IF (KRD(5).EQ.' ') THEN
         OLON=ID(36) + ID(37)/60. + (ID(38)/FLOAT(ID(68)))/3600.
      END IF

C     Set flag for aircraft coordinates: (0) XY, (1) LatLon (default)
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
C           Grid is XY and Aircraft positions are XY
C
            TITL_INP='Inp-XY='
            TITL_OUT='Out-XY='
            CONVERSION='NONE'
         ELSE
C           Grid is XY and Aircraft positions are LL
C
            TITL_INP='Inp-LL='
            TITL_OUT='Out-XY='
            CONVERSION='LatLon --> XY'
         END IF

      ELSE
         ISWAP=1
         IF(ILAT.EQ.0)THEN
C           Grid is LL and Aircraft positions are XY
C
            TITL_INP='Inp-XY='
            TITL_OUT='Out-LL='
            CONVERSION='XY --> LonLat'
         ELSE
C           Grid is LL and Aircraft positions are LL
C
            TITL_INP='Inp-LL='
            TITL_OUT='Out-LL='
            CONVERSION='LatLon --> LonLat'
         END IF
      END IF

 15   CALL KARDIN(KRD)
      IF (KRD(2).EQ.'WINDS') THEN
C
C     WINDS WILL BE PLOTTED ALONG TRACK TOO; PARSE IN INFO
C
         READ(KRD,25)FU,FV,FW
 25      FORMAT(//F8.0/F8.0/F8.0)
         IU=INT(FU)
         IV=INT(FV)
         IW=INT(FW)
         IMAX=MAX(IU,IV,IW)
         IF (IMAX.LE.0) THEN
            WRITE(*,35)
 35         FORMAT(/,5X,'+++ WIND COMPONENT POSITIONS IN ASCII FILE ',
     X           'MUST BE SPECIFIED. WINDS NOT READ IN. +++')
         ELSE
            IF (IU.NE.0 .AND. IV.NE.0 .AND. IW.EQ.0) THEN
               IACTWND=1
            ELSE IF (IV.NE.0 .AND. IW.NE.0 .AND. IU.EQ.0) THEN
               IACTWND=2
            ELSE IF (IU.NE.0 .AND. IW.NE.0 .AND. IV.EQ.0) THEN
               IACTWND=3
            ELSE IF (IU.NE.0 .AND. IV.NE.0 .AND. IW.NE.0) THEN
               IACTWND=4
            ELSE
               WRITE(*,40)
 40            FORMAT(/,5X,' +++NEED AT LEAST TWO WIND COMPONENTS+++')
               IACTWND=0
            END IF
         END IF
         GOTO 15
      ELSE IF (KRD(2).EQ.'GRID') THEN
         IGRID=1
         IF (IU.NE.0) NAMOUT(1)=KRD(3)
         IF (IV.NE.0) NAMOUT(2)=KRD(4)
         IF (IW.NE.0) NAMOUT(3)=KRD(5)
         GOTO 15
      ELSE IF (KRD(2).EQ.'ADVECT') THEN
         READ(KRD(3),30)ASPD
 30      FORMAT(F8.0)
         READ(KRD(4),30)ADIR
         READ(KRD(5),30)ATIM
         IF (ATIM.EQ.0.0) ATIM=TMV
         GOTO 15
      ELSE IF (KRD(1).EQ.' '  ) THEN
         WRITE(*,17)
 17      FORMAT(/,'+++ SECOND FIELD MUST BE "WINDS" OR STACK',
     X        ' MUST BE TERMINATED BY AN END CARD +++'/)
         RETURN
      ELSE IF (KRD(1).NE.'END') THEN
         WRITE(*,5)
 5       FORMAT(/,'+++ READAIR MUST BE TERMINATED BY AN END CARD +++'/)
         RETURN
      END IF
      NACT=0
      IMIDNGT=0
      PSEC=0.0
      ANGXAX=ID(40)/FLOAT(ID(69))

CThe following statements were added to open the file correctly
Cin HP unix.  SMF Nov 2000.  The old open is commented out.
C     05/09/2001 - ljm fix error (itwo = iunit - 10)

      FILENAME = 'fort.'
      if(iunit .LT. 10) then
         FILENAME(6:6) = char(iunit + 48)
      end if
      if(iunit .ge. 10 .and. iunit .lt. 100) then
         ione = iunit/10
         itwo = iunit - 10*ione
         FILENAME(6:6) = char(ione + 48)
         FILENAME(7:7) = char(itwo + 48)
      endif
      
      OPEN(UNIT = IUNIT, FILE = FILENAME, IOSTAT = IOVAL)

c      OPEN(UNIT=IUNIT,FORM='FORMATTED',ACCESS='SEQUENTIAL',
c     X     IOSTAT=IOVAL)
      IF (IOVAL.NE.0) THEN
         CALL CEDERX(584,1)
         RETURN
      END IF
C
C     SUMMARIZE COMMAND
C
      WRITE(*,100)IUNIT,OLAT,OLON,DELACT,BTIME,ETIME,COORDDES(ILAT+1),
     X     PLOTDES(JLAT+1),CONVERSION
 100  FORMAT(/,5X,'AIRCRAFT DATA INPUT PARAMETERS...'/,
     X         5X,'                                 UNIT:',I4,/
     X         5X,'                                 OLAT:',F9.4,/
     X         5X,'                                 OLON:',F9.4,/
     X         5X,'                       RESOLUTION (S):',F8.1,/
     X         5X,'      BEG TIME OF DATA TO BE USED (S):',F8.1,/
     X         5X,'      END TIME OF DATA TO BE USED (S):',F8.1,/
     X         5X,'      AIRCRAFT POSITIONS SPECIFIED AS:',2X,A14,/
     X         5X,'           PLOTTING GRID SPECIFIED AS:',2X,A14,/
     X         5X,'                           CONVERSION:',2x,A20/)
      IF (ASPD.NE.0.0) THEN
         WRITE(*,125)ASPD,ADIR,ATIM
 125     FORMAT(5X,'                ADVECTION SPEED(M/S):',F8.2/
     X          5X,'DIRECTION FROM WHICH STORM IS COMING:',F8.2/
     X          5X,'                      ANCHOR TIME(S):',F8.2)
      END IF
      IF (IGRID.EQ.1) THEN
         WRITE(*,150)NAMOUT(1),NAMOUT(2),NAMOUT(3)
 150     FORMAT(5X,'                FIELDS BEING GRIDDED:',3A8/)
      END IF

C
C     READ THE DATA IN
C
      PRINT *,'Open IUNIT,FILENAME=',iunit,' ',filename
      PRINT *,'IACTWND,BTIME,ETIME=',IACTWND,BTIME,ETIME
      PRINT *,' '
 300  IF (IACTWND.NE.0) THEN
         READ(IUNIT,*,END=50)ISEC,XorLAT,YorLON,ZMSL,
     X        (WIND(I),I=1,(IMAX-4))
         SEC=REAL(ISEC)
         IF ((SEC-PSEC).LT.0.0) THEN
C     MIDNIGHT HAS BEEN CROSSED
            IMIDNGT=IMIDNGT + 3600*24
         END IF
         SEC=SEC+IMIDNGT
         IF (SEC.LT.BTIME) GOTO 300
         IF (SEC.GT.ETIME) GOTO 50
      ELSE
         READ(IUNIT,*,END=50)ISEC,XorLAT,YorLON,ZMSL
         SEC=REAL(ISEC)
         SEC=SEC+IMIDNGT
         IF ((SEC-PSEC).LT.0.0) THEN
C     MIDNIGHT HAS BEEN CROSSED
            IMIDNGT=IMIDNGT + 3600*24
            PRINT *,'***MIDNIGHT HAS BEEN CROSSED: btime,etime=',
     X           btime,etime
            PRINT *,'***sec,isec,psec=',sec,isec,psec
            SEC=SEC+3600*24
         END IF
         IF (SEC.LT.BTIME) GOTO 300
         IF (SEC.GT.ETIME) GOTO 50
      END IF

      IF (NACT.EQ.0) BEGACT=SEC
c-----IF (MOD(INT(SEC-BEGACT),INT(DELACT)).EQ.0) THEN

         IF(JLAT.EQ.0)THEN
            IF(ILAT.EQ.0)THEN
C              Grid is XY and Aircraft positions are XY
C
               XMRK=XorLAT
               YMRK=YorLON
            ELSE
C              Grid is XY and Aircraft positions are LL
C
               IF (CDIR.EQ.'EAST') YorLON=-YorLON
               CALL LL2XYDRV(XorLAT,YorLON,X,Y,OLAT,OLON,ANGXAX)
               XMRK=X
               YMRK=Y
            END IF

         ELSE
            IF(ILAT.EQ.0)THEN
C              Grid is LL and Aircraft positions are XY
C
               CALL XY2LLDRV(PLAT,PLON,XorLAT,YorLON,OLAT,OLON,ANGXAX)
               IF (CDIR.EQ.'EAST') PLON=-PLON
               XMRK=PLON
               YMRK=PLAT
            ELSE
C           Grid is LL and Aircraft positions are LL
C
               XMRK=YorLON
               YMRK=XorLAT
            END IF
         END IF

         NACT=NACT+1
         IF (NACT.GT.MXL) THEN
            NACT=NACT-1
            WRITE(*,45)MXL
 45         FORMAT(/' +++TOO MANY AIRCRAFT POSITIONS. MAX=',I6,
     X           ' +++'/)
            GOTO 50
         END IF
         XACT(NACT)=XMRK
         YACT(NACT)=YMRK
      
         ZACT(NACT)=ZMSL/1000.
         IF (IU.GT.0) UACT(NACT)=WIND(IU-4)
         IF (IV.GT.0) VACT(NACT)=WIND(IV-4)
         IF (IW.GT.0) WACT(NACT)=WIND(IW-4)
         PSEC=SEC

C        List the aircraft input information
C
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
         ISEC=INT(SEC)
         IHHMMSS=IHR*10000+IMN*100+ISC
c         IF(MOD(ISEC,10).EQ.0)THEN
            WRITE(6,1770)ISEC,IHR,IMN,ISC,TITL_INP,XorLAT,YorLON,
     X           TITL_OUT,XACT(NACT),YACT(NACT),ZMSL
 1770       FORMAT(2X,'Time=',I5,'=',3I2.2,2X,A8,2F12.4,2X,A8,
     X           2F12.4,4X,'Z(m)=',F8.3)
c         END IF
c-----END IF

C
C     GO READ NEXT RECORD
C
      GOTO 300


 50   CONTINUE
      print *,'EOF on iunit=',iunit
      PRINT *,' '
      CLOSE(IUNIT)

      IF (ASPD.NE.0.0) THEN
C
C     ADVECT DATA 
C
         ANGXAX=FLOAT(ID(40))/FLOAT(ID(69))
         DIR=(ADIR+(90.-ANGXAX)+180.)*DTR
         UADV=ASPD*SIN(DIR)*.001
         VADV=ASPD*COS(DIR)*.001
         DO I=1,NACT
            XACT(I)=XACT(I)-((I-1)*DELACT+BEGACT-ATIM)*UADV
            YACT(I)=YACT(I)-((I-1)*DELACT+BEGACT-ATIM)*VADV
         END DO
      END IF

C     ROTATE VECTORS, IF NEEDED
      IF (IU.GT.0 .AND. IV.GT.0 .AND. ANGXAX.NE.90.0) THEN
         THETA=(ANGXAX-90.0)*DTR
         DO I=1,NACT
            U=UACT(I)*COS(THETA) - VACT(I)*SIN(THETA)
            V=UACT(I)*SIN(THETA) + VACT(I)*COS(THETA)
            UACT(I)=U
            VACT(I)=V
         END DO
      END IF
      

C     GRID WIND DATA, IF REQUESTED
      NST=0
      IF (IGRID.EQ.1 .AND. IACTWND.NE.0) CALL GRIDAIR(XACT,YACT,ZACT,
     X     UACT,VACT,WACT,NACT,RBUF,IBUF,BEGACT,DELACT,NAMOUT,NST)

      RETURN

      END






