
      PROGRAM TST_LMA

      PARAMETER (MXLM=250000)
      DIMENSION XLMA(MXLM),YLMA(MXLM),ZLMA(MXLM)
      DOUBLE PRECISION TLMA(MXLM)
      DIMENSION HLMA(MXLM),AZLMA(MXLM)
      CHARACTER*8 INDAT(10)

      COMMON /HEMISPHERE/ LATSPHERE,LONSPHERE,LAT_SIGN,LON_SIGN
      CHARACTER*8 LATSPHERE,LONSPHERE
      REAL LAT_SIGN,LON_SIGN
      DATA LATSPHERE,LONSPHERE/'NORTH','WEST'/
      DATA LAT_SIGN,LON_SIGN/+1.0,+1.0/
C
C     getlma.inp:
C-------*-------*-------*-------*-------*-------*-------*-------*-------*-------
CGETLMA LYLOUT_000712_205000_0600.dat   WEST    39.76114102.0935        90.0 0.5
C       -80.0   -30.0   -90.0   -40.0
C
      OLAT = 0.0
      OLON = 0.0
      ANGXAX=90.0

      OPEN(UNIT=5,FILE='getlma.inp',STATUS='OLD')
      READ(5,3)RLMA
 3    FORMAT(8X,F8.0)
      LMA=INT(RLMA)

      DO L = 1,LMA
         print *,'LMA file #=',L
 5       READ(5, 7)(INDAT(I),I=1,10)
 7       FORMAT(10A8)
         WRITE(6, 7)(INDAT(I),I=1,10)
         READ(5, 9)XMIN,XMAX,YMIN,YMAX
 9       FORMAT(8X,4F8.0)
         print *,'Area XmnXmx=',xmin,xmax,' YmnYmx=',ymin,ymax

C     GET LMA Lightning Channel (LAT,LON) POSITIONS FOR LATER OVERLAYING
C
 240     CALL GETLMA(INDAT,TLMA,XLMA,YLMA,ZLMA,HLMA,AZLMA,MXLM,ILMA,
     X        OLAT,OLON,ANGXAX,DTLMA,ORLAT,ORLON,DZLMA,
     X        XMIN,XMAX,YMIN,YMAX)
      END DO
      STOP
      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE GETLMA(INDAT,TLMA,XLMA,YLMA,ZLMA,HLMA,AZLMA,MXLM,
     X     ILMA,OLAT,OLON,ANGXAX,DTLMA,ORLAT,ORLON,DZLMA,
     X     XMIN,XMAX,YMIN,YMAX)
C
C  READ IN LMA LIGHTNING CHANNEL (TIME-LAT-LON-HEIGHT) FROM LATFILE:
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
C     TLMA      - Time of the strike
C     XLMA,YLMA - (X,Y) position of landmark relative to origin
C     ZLMA      - Height of LMA lightning channel
C     HLMA      - Horizontal range (km) of lightning channel
C     AZLMA     - Azimuth angle (deg) of lightning channel
C     DTLMA     - Time interval for plotting all strikes between
C                 +/-DTLMA sec of central time of current scan
C     DZLMA     - Height/Azimuth interval for plotting all lightning points
C
      DIMENSION XLMA(MXLM),YLMA(MXLM),ZLMA(MXLM)
      DOUBLE PRECISION TLMA(MXLM)
      DIMENSION HLMA(MXLM),AZLMA(MXLM)
      CHARACTER LATFILE*32,IDIR*4,ORIG*4
      CHARACTER*60 HEADER
      CHARACTER*8 INDAT(10)
      CHARACTER*8 BLANK
      DOUBLE PRECISION SEC,FSEC,RLAT,RLON

c      DATA ILMA_MOD/1000/
      DATA ILMA_MOD/1/
      DATA BLANK/'        '/
      DATA TORAD,TODEG/0.017453293,57.29577951/

      IF(INDAT(9).EQ.BLANK)THEN
         READ(INDAT,3)IDIR,ORIG,DTLMA,DZLMA
 3       FORMAT(/////A4,4X/A4,4X//F4.0,F4.0)
      ELSE
         READ(INDAT,4)IDIR,ORIG,ANGXAX,DTLMA,DZLMA
 4       FORMAT(/////A4,4X/A4,4X/F8.0/F4.0,F4.0)
      END IF
      IF(DTLMA.LE.0.0)DTLMA=30.0
      IF(DZLMA.LE.0.0)DZLMA=1.0
C
C     SPOL      39.76114   -102.09347 1103.0
C     CHIL      39.234605  -102.27791 1285.0
C     KGLD      39.36694   -101.70027 1122.8
C
      IF(ORIG.EQ.'    ')ORIG = 'KGLD'
      IF(ORIG.EQ.'SPOL')THEN
         ORLAT = 39.76114
         ORLON = -102.09347
      ELSE IF(ORIG.EQ.'CHIL')THEN
         ORLAT = 39.234605  
         ORLON = -102.27791
      ELSE IF(ORIG.EQ.'KGLD')THEN
         ORLAT = 39.36694
         ORLON = -101.70027
      END IF
      print *,'ORIGIN = ',orig,orlat,orlon

      IF(OLAT.EQ.0.0 .AND. ORLAT.NE.0.0)OLAT=ORLAT
      IF(OLON.EQ.0.0 .AND. ORLON.NE.0.0)OLON=ORLON
      IF(IDIR.EQ.'WEST')THEN
         OLON=+1.0*ABS(OLON)
      ELSE
         OLON=-1.0*ABS(OLON)
      END IF

      LATFILE=INDAT(2)//INDAT(3)//INDAT(4)//INDAT(5)
      WRITE(6,5)LATFILE,IDIR,OLAT,OLON,ANGXAX
 5    FORMAT(1X,'GETLMA: IN-CLOUD LIGHTNING POSITION FILE= ',A32,A4,4X,
     X     2F12.5,F8.1)
      OPEN(UNIT=9,FILE=LATFILE,STATUS='OLD')
      
      ILMA=0
 10   READ(9,11,END=20)HEADER
 11   FORMAT(A60)
      IF(HEADER(1:1).NE.' ')THEN
         print *,'SKIP: ',header
         GO TO 10
      END IF
      READ(HEADER,12,ERR=10,END=20)SEC,RLAT,RLON,HGHT,CHISQ,NSTA
 12   FORMAT(F16.9,F11.6,F12.6,F8.1,F6.2,I3)

      IF(CHISQ.GT.20.0)GO TO 10

      ILMA=ILMA+1
      IF(ILMA.GT.MXLM)THEN
         ILMA=ILMA-1
         CLOSE (UNIT=9)
         RETURN
      END IF
      IHR = SEC/3600.0
      IMN = (SEC - IHR*3600.0)/60.0
      ISC = (SEC - IHR*3600.0 - IMN*60.0)
      FSEC= SEC - FLOAT(INT(SEC))

      IF(IDIR.EQ.'WEST')THEN
         PLON=+1.0*ABS(RLON)
      ELSE
         PLON=-1.0*ABS(RLON)
      END IF
      PLAT=RLAT
      CALL LL2XYDRV(PLAT,PLON,X,Y,OLAT,OLON,ANGXAX)
      XLMA(ILMA)=X
      YLMA(ILMA)=Y
      TLMA(ILMA)=SEC
      ZLMA(ILMA)=0.001*HGHT
      Z=0.001*HGHT
      HRNG=SQRT(X*X+Y*Y)

      IF(Y.EQ.0.0.AND.X.EQ.0.0)THEN
         AZ=180.0
      ELSE IF(Y.EQ.0.0.AND.X.GT.0.0)THEN
         AZ=90.0
      ELSE IF(Y.EQ.0.0.AND.X.LT.0.0)THEN
         AZ=270.0
      ELSE
         AZ=TODEG*ATAN2(X,Y)
      END IF
      IF(AZ.LT.0.0)THEN
         AZIM=AZ+360.0
      ELSE
         AZIM=AZ
      END IF
      HLMA(ILMA)=HRNG
      AZLMA(ILMA)=AZIM

      IF(RLAT.LT.XMIN .OR. RLAT.GT.XMAX)GO TO 10
      IF(RLON.LT.YMIN .OR. RLON.GT.YMAX)GO TO 10

      IF(ILMA.EQ.1 .OR. MOD(ILMA,ILMA_MOD).EQ.0)THEN
         WRITE(6,15)ILMA,IHR,IMN,ISC,FSEC,PLAT,PLON,HGHT,X,Y,Z,
     X        HRNG,AZIM
 15      FORMAT(1X,'I,HMS,LL,H=',I8,1X,I2.2,':',I2.2,':',I2.2,
     X        F12.9,2F12.6,F8.1,'  X,Y,Z=',3F8.2,' HRNG,AZ=',2F8.2)
      END IF
      GO TO 10

 20   CONTINUE
      print *,'Closing LMA lightning channel location file'
      print *,'###########################################'
      print *,' '
      CLOSE (UNIT=9)
      RETURN
      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE LL2XYDRV(PLAT_IN,PLON_IN,X,Y,ORLAT_IN,ORLON_IN,ANGXAX)
C
C     DRIVER ROUTINE FOR CONVERTING THE SEPARATION OF TWO POINTS
C     SPECIFIED IN LAT, LON TO SEPARATION IN X, Y IN KM. 
C     THIS ROUTINE (AND LL2XY) WILL WORK FOR POINTS IN ANY PART OF
C     THE GLOBE WITH SOME RESTRICTIONS (SEE BELOW). THE ANGLE CONVENTIONS ARE:
C     0   < LAT < 90   ==>  NORTHERN HEMISPHERE
C     -90 < LAT < 0    ==>  SOUTHERN HEMISPHERE
C     
C     0    < LON < 180 ==>  WESTERN HEMISPHERE
C     -180 < LON < 0   ==>  EASTERN HEMISPHERE
C     
C     PLAT  - LAT. OF POINT FOR WHICH X,Y IS DESIRED
C     PLON  - LON. OF POINT FOR WHICH X,Y IS DESIRED
C     X     - OUTPUT X VALUE RELATIVE TO ORLAT, ORLON IN KM
C     Y     - OUTPUT Y VALUE RELATIVE TO ORLAT, ORLON IN KM
C     ORLAT - LAT. OF ORIGIN
C     ORLON - LON. OR ORIGIN
C     ANGXAX- ANGLE OF X-AXIS REL. TO TRUE NORTH (USUALLY 90.0)
C
C     KNOWN RESTRICTIONS AND LIMITATIONS:
C
C     1) ||PLAT| - |ORLAT|| <= 90.0
C     2) ||PLON| - |ORLON|| <= 90.0
C     3) NO INPUT LAT OR LON VALUE SHOULD BE EQUAL TO EXACTLY ZERO
C     4) THE CODE IS NOT SETUP TO HANDLE CROSSING OVER BOTH HEMISPHERE 
C     BOUNDARIES AT ONCE; IT CAN HANDLE CROSSING EITHER THE NORTH/SOUTH 
C     HEMISPHERE BOUNDARY OR THE WEST/EAST BOUNDARY, BUT NOT BOTH AT ONCE. 
C     FOR EXAMPLE, YOU CAN'T HAVE AN ORIGIN AT (1.0 deg, 1.0 deg) AND TRY 
C     TO FIND THE X,Y OF A POINT AT (-1.0 deg, -1.0 deg). YOU COULD FIND 
C     THE X,Y OF A POINT AT (-1.0 deg, 1.0 deg), HOWEVER.
C     5) CODE WON'T WORK IF YOU TRY TO CROSS A POLE
C
C     Note: Precision of lat/lon in deg = 0.0001 or rad = 0.0001*pi/180
C           This is about 10m with R_e=6380 km.  Don't bother with 
C           transformations if lat/lon are less than about 1.75E-06
C      
      PARAMETER (EPS=1.0E-07, DEGRAD=0.01745329)
C
C     COMMON block variables returned from LAT_LON
C
      COMMON /HEMISPHERE/ LATSPHERE,LONSPHERE,LAT_SIGN,LON_SIGN
      CHARACTER*8 LATSPHERE,LONSPHERE
      REAL LAT_SIGN,LON_SIGN
      
      ORLAT = LAT_SIGN*ABS(ORLAT_IN)
      ORLON = LON_SIGN*ABS(ORLON_IN)
      PLAT  = LAT_SIGN*ABS(PLAT_IN)
      PLON  = LON_SIGN*ABS(PLON_IN)

      ICROSS=0
C
C     DETERMINE IF A HEMISPHERE BOUNDARY HAS BEEN CROSSED
C
      IF (SIGN(1.0,ORLAT).GT.SIGN(1.0,PLAT)) ICROSS=1
      IF (SIGN(1.0,ORLAT).LT.SIGN(1.0,PLAT)) ICROSS=2
      IF (SIGN(1.0,ORLON).GT.SIGN(1.0,PLON)) THEN
         IF (ICROSS.NE.0) THEN
            WRITE(*,10)
 10         FORMAT(/,5X,'+++ CANNOT HANDLE DUAL HEMISPHERE CROSSOVER ',
     X           '+++')
            STOP
         ELSE
            ICROSS=3
         END IF
      ELSE IF (SIGN(1.0,ORLON).LT.SIGN(1.0,PLON)) THEN
         IF (ICROSS.NE.0) THEN
            WRITE(*,10)
            STOP
         ELSE
            ICROSS=4
         END IF
      END IF
         
      IF (ORLAT.GE.0.0) THEN
         INHEM=1
      ELSE
         INHEM=0
      END IF
      IF (ORLON.GE.0.0) THEN
         IWHEM=1
      ELSE
         IWHEM=0
      END IF
      IF (ICROSS.EQ.0) THEN
C
C     NO HEMISPHERE CROSSOVER; JUST CALL LL2XY
C
      
C
C     MAKE SIGNED VALUES POSITIVE 
C
         DEGLAT=ABS(PLAT)
         DEGLON=ABS(PLON)
         SWLAT =ABS(ORLAT)
         SWLON =ABS(ORLON)         
         
         CALL LL2XY(DEGLAT,DEGLON,X,Y,SWLAT,SWLON)

C
C     SWITCH SIGNS IF NOT IN NORTHERN OR WESTERN HEMISPHERES
C
         IF (INHEM.EQ.0) Y=-Y
         IF (IWHEM.EQ.0) X=-X

      ELSE IF (ICROSS.EQ.1) THEN
C
C     +LAT -> -LAT
C

         DEGLAT=EPS
         DEGLON=ABS(PLON)
         SWLAT =ABS(ORLAT)
         SWLON =ABS(ORLON)

         CALL LL2XY(DEGLAT,DEGLON,X1,Y1,SWLAT,SWLON)
         
         IF (IWHEM.EQ.0) X1=-X1

         DEGLAT=ABS(PLAT)
         DEGLON=ABS(PLON)
         SWLAT =EPS
         SWLON =ABS(PLON)
         CALL LL2XY(DEGLAT,DEGLON,X2,Y2,SWLAT,SWLON)
         
         Y=-(ABS(Y1)+ABS(Y2))
         X=X1
         
      ELSE IF (ICROSS.EQ.2) THEN
C
C     -LAT -> +LAT
C
         DEGLAT=EPS
         DEGLON=ABS(PLON)
         SWLAT =ABS(ORLAT)
         SWLON =ABS(ORLON)

         CALL LL2XY(DEGLAT,DEGLON,X1,Y1,SWLAT,SWLON)
         
         IF (IWHEM.EQ.0) X1=-X1

         DEGLAT=ABS(PLAT)
         DEGLON=ABS(PLON)
         SWLAT =EPS
         SWLON =ABS(PLON)
         CALL LL2XY(DEGLAT,DEGLON,X2,Y2,SWLAT,SWLON)
         
         Y=(ABS(Y1)+ABS(Y2))
         X=X1
         
      ELSE IF (ICROSS.EQ.3) THEN
C
C     +LON -> -LON
C
         DEGLAT=ABS(PLAT)
         IF (ABS(PLON).LE.45.0) THEN
            DEGLON=EPS
         ELSE IF (ABS(PLON).GE.135.0) THEN
            DEGLON=180.0-EPS
         END IF
         SWLAT =ABS(ORLAT)
         SWLON =ABS(ORLON)

         CALL LL2XY(DEGLAT,DEGLON,X1,Y1,SWLAT,SWLON)

         IF (INHEM.EQ.0) Y1=-Y1

         DEGLAT=ABS(PLAT)
         DEGLON=ABS(PLON)
         SWLAT =ABS(PLAT)
         IF (ABS(PLON).LE.45.0) THEN
            SWLON=EPS
         ELSE IF (ABS(PLON).GE.135.0) THEN
            SWLON=180.0-EPS
         END IF

         CALL LL2XY(DEGLAT,DEGLON,X2,Y2,SWLAT,SWLON)
         
         Y=Y1
         X=(ABS(X1)+ABS(X2))

      ELSE IF (ICROSS.EQ.4) THEN
C
C     -LON -> +LON
C
         DEGLAT=ABS(PLAT)
         IF (ABS(PLON).LE.45.0) THEN
            DEGLON=EPS
         ELSE IF (ABS(PLON).GE.135.0) THEN
            DEGLON=180.0-EPS
         END IF
         SWLAT =ABS(ORLAT)
         SWLON =ABS(ORLON)

         CALL LL2XY(DEGLAT,DEGLON,X1,Y1,SWLAT,SWLON)

         IF (INHEM.EQ.0) Y1=-Y1

         DEGLAT=ABS(PLAT)
         DEGLON=ABS(PLON)
         SWLAT =ABS(PLAT)
         IF (ABS(PLON).LE.45.0) THEN
            SWLON=EPS
         ELSE IF (ABS(PLON).GE.135.0) THEN
            SWLON=180.0-EPS
         END IF

         CALL LL2XY(DEGLAT,DEGLON,X2,Y2,SWLAT,SWLON)
         
         Y=Y1
         X=-(ABS(X1)+ABS(X2))
         
      END IF

C
C     ROTATE, IF NECESSARY
C
      IF (ANGXAX.NE.90.0) THEN
         THETA=(ANGXAX-90.0)*DEGRAD
         XT=X
         YT=Y
         X=XT*COS(THETA) - YT*SIN(THETA)
         Y=XT*SIN(THETA) + YT*COS(THETA)
      END IF
      

      RETURN

      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE  LL2XY (DEGLAT, DEGLON, X, Y, SWLAT, SWLON)
C
C  TO CONVERT LAT.,LONG. TO X,Y IN KM WITH RESPECT TO SWLAT,SWLON
C  PASCAL BY P. JOHNSON, 17-FEB-81.  FORTRAN TRANS R. VAUGHAN 9/81.
C  FINAL REPAIR, M. BRADFORD, 4/88
C  WARNING!  WORKS ONLY IN NORTHERN/WESTERN HEMISPHERES!
C
C     Uses spherical trigonometry associated with a 
C     great circle through the origin and the point. 
C
C     COSN - cosine of heading angle from origin --> point
C     S    - distance in plane tangent to origin
C          - distance along Earth's surface is R*(N*PI/180)
C            where N = arcosin(COSN) in deg
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL DEGLAT,DEGLON,X,Y,SWLON,SWLAT

      PARAMETER (PI=3.141592654)
      PARAMETER (R=6380.12)
      PARAMETER (DEGRAD=0.01745329)
      PARAMETER (RADDEG=57.2957795)
      PARAMETER (EPS=1.0E-07)

      X=0.0
      Y=0.0
      ALAT = SWLAT * DEGRAD
      CALAT = COS(ALAT)
      SALAT = SIN(ALAT)
      ALONG = ABS(SWLON * DEGRAD)
      BLAT = DEGLAT * DEGRAD
      BLONG = ABS(DEGLON * DEGRAD)
      CBLAT = COS(BLAT)
      SBLAT = SIN(BLAT)
      DLON = ALONG-BLONG
      DLAT = ABS(DEGLAT-SWLAT)*DEGRAD
      IF(DLAT.LT.EPS .AND. ABS(DLON).LT.EPS) GO TO 90
      CDLON = COS(DLON)
C
C     Both ALAT and BLAT = 0 results in divide by 0 for AZA and AZB.
C
c     debugging print statements
c
c      write(*,*)'denom aza=',nint(calat),nint(sblat),
c     +                       nint(cblat),nint(salat),nint(cdlon)
c      write(*,*)'          ',calat*sblat/cblat,salat*cdlon
c      write(*,*)'denom azb=',nint(cblat),nint(salat),
c     +                       nint(calat),nint(sblat),nint(cdlon)
c      write(*,*)'          ',cblat*salat/calat,sblat*cdlon
c      write(*,*)'dlon=',dlon*raddeg
c      write(*,*)'salat*sblat=',salat*sblat
c      write(*,*)'calat*cblat*cdlon=',calat*cblat*cdlon
      DEN=(CALAT*SBLAT/CBLAT-SALAT*CDLON)
      IF(ABS(DEN).LT.EPS)THEN
         AZA =  90.0*DEGRAD
         AZB = -90.0*DEGRAD
      ELSE
         AZA = ATAN(SIN( DLON)/(CALAT*SBLAT/CBLAT-SALAT*CDLON))
         AZB = ATAN(SIN(-DLON)/(CBLAT*SALAT/CALAT-SBLAT*CDLON))
      END IF
C
C  GET BEARING
C
      IF(BLAT .LT. ALAT) AZA = AZA+PI
      IF(ALAT .LT. BLAT) AZB = AZB+PI
      IF(AZA .LT. 0) AZA = AZA + 2.*PI
      IF(AZB .LT. 0) AZB = AZB + 2.*PI
      IF(DLON.LT.0.0 .AND. (AZA.GT.0  .AND. AZA.LT.PI))THEN
         AZA = AZA+PI
      END IF
      IF(DLON.GT.0.0 .AND. (AZA.GT.PI .AND. AZA.LT. 2.*PI))THEN
         AZA = AZA-PI
      END IF
C
      COSN = SALAT*SBLAT + CALAT*CBLAT*CDLON
      IF(DLON.LT.EPS)THEN
         S = R * ACOS(COSN)
      ELSE
         SINN = SIN(DLON) * SIN(PI/2.0-BLAT) / SIN(AZA)
         S = R * ATAN(SINN/COSN)
      END IF
      X = S * SIN(AZA)
      Y = S * COS(AZA)
c      write(*,*)'s,aza=',s,aza*raddeg
C
   90 CONTINUE
      RETURN
C
      END
