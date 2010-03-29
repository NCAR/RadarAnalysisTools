      program tst_rvtopo

      COMMON /HEMISPHERE/ LATSPHERE,LONSPHERE,LAT_SIGN,LON_SIGN
      CHARACTER*8 LATSPHERE,LONSPHERE
      REAL LAT_SIGN,LON_SIGN

      parameter (ni=201,nj=201)
      DIMENSION VEX(NI,NJ),CSP(3,3)

      logical latlon
      data latlon/.true./

      latsphere='NORTH'
      lonsphere='EAST'
      lat_sign=1.0
      lon_sign=-1.0

      xmin=-1.0
      xd=0.1
      xmax=1.0
      ymin=-1.0
      yd=0.1
      ymax=1.0
      reflat=47.90345
      reflon=7.52887
      angxax=90.0
      
      csp(1,1)=xmin
      csp(2,1)=xd
      csp(3,1)=xmax
      csp(1,2)=ymin
      csp(2,2)=yd
      csp(3,2)=ymax
      zip=-100.0
      i1=1
      i2=21
      j1=1
      j2=21

      CALL RVTOPO(VEX,NI,NJ,I1,I2,J1,J2,CSP,LATLON,REFLAT,REFLON,
     X     ANGXAX,ZIP)

      do j=21,1,-1
         write(6,13)j,(vex(i,j),i=1,21)
         write(7,13)j,(vex(i,j),i=1,21)
 13      format(' j=',i4,' vex=',21f6.2)
      end do
      stop
      end
c
c----------------------------------------------------------------------X
c
      SUBROUTINE RVTOPO(VEX,NI,NJ,I1,I2,J1,J2,CSP,LATLON,REFLAT,REFLON,
     X     ANGXAX,ZIP)

c     RVTOPO: F(OUT)=Topographic heights for the Rhine Valley in Germany.
c                    Heights (m) are read from unit 8 and interpolated
c                    to the current Cedric volume grid.
c
c     Returns VEX: terrain heights in decameters above mean sea level.
c
c     Output topography is in km, with missing data set to ZIP.  The input
c     file has 480 longitude values (6-10 deg East) x 360 latitude values
c    (47-50 deg North).  Cell sizes are (4 deg/480)=(3 deg/360)=0.00833333
c     deg in both the longitude (x) and latitude (y) directions which is
c     to say that each terrain height represents an "average" height in
c     each cell centered at the (longitude, latitude(x,y) location. 
c
c	There are 480 columns and 360 rows of numbers.  The lower left-hand
c     corner is at 6 deg E longitude (x) and 47 deg N latitude (y).  
c	Columns (i) run from 6-10 deg latitude and rows (j) run from 47-50
c     deg latitude.
c
c                i==> 1 to 480 (Longitude-->x)
c	50-+---------|---------|---------|---------+ j=360 (Latitude-->y)
c	   |         |         |         |         |    
c	   |         |         |         |         |    
c	   |         |         |         |         |    
c	   |         |         |         |         |    
c  L	49-+---------+---------+---------+---------+
c  A	   |         |         |         |         |    
c  T	   |         |         |         |         |    
c  I	   |         |         |         |         |    
c  T	   |         |         |         |         |    
c  U	48-+---------+---------+---------+---------+
c  D	   |         |         |         |         |    
c  E	   |         |         |         |         |    
c (Y)	   |         |         |         |         |    
c	   |         |         |         |         |    
c	47-+---------|---------|---------|---------+  j=001 (Latitude-->y)
c        6         7         8         9        10
c                  Longitude (X)
c
c     REFLAT,REFLON - Origin latitude and longitude
c     ANGXAX        - Azimuth angle of +X-axis
c     ZGG           - Array to hold ASCII values of terrain heights (m)
c
      PARAMETER(MX_LON=480,MY_LAT=360)
      DIMENSION ZGG(MX_LON,MY_LAT)
      DIMENSION VEX(NI,NJ),CSP(3,3)
      LOGICAL LATLON

      double precision latitude, longitude, del
      real lat_ll,lon_ll,lat_lr,lon_lr
      real lat_ul,lon_ul,lat_ur,lon_ur
      real lat_del,lon_del
      
      data lat_ll,lon_ll/47.0,6.0/
      data lat_lr,lon_lr/47.0,10.0/
      data lat_ul,lon_ul/50.0,6.0/
      data lat_ur,lon_ur/50.0,10.0/


C     LINEAR INTERPOLATION FORMULA FOR FUNCTION AT (X1.LE.X.LE.X2)
C
C
      FINT(X1,F1,X2,F2,X)=F1+(F2-F1)*(X-X1)/(X2-X1)
      
C     Print out the input variables to RVTOPO
C
      print *,'RVTOPO: ',reflat,reflon,angxax
      print *,'RVTOPO: ',ni,nj,i1,i2,j1,j2
      print *,'RVTOPO: x=',CSP(1,1),CSP(3,1),CSP(2,1)
      print *,'RVTOPO: y=',CSP(1,2),CSP(3,2),CSP(2,2)
      print *,'RVTOPO: zip=',zip

      ORLAT = REFLAT
      ORLON = REFLON
      BDVAL = ZIP
      XMIN  = CSP(1,1)
      XD    = CSP(2,1)
      XMAX  = CSP(3,1)
      YMIN  = CSP(1,2)
      YD    = CSP(2,2)
      YMAX  = CSP(3,2)
      print *,'xmin,xmax,xd=',xmin,xmax,xd
      print *,'ymin,ymax,yd=',ymin,ymax,yd
      
c    ul------ur
c     |       |
c     |       |
c     |       |
c    ll------lr
c
      TX1 = lon_ll
      TX2 = lon_lr
      TY1 = lat_ll
      TY2 = lat_ul
      lon_del = (lon_ll-lon_lr)/float(mx_lon)
      lat_del = (lat_ul-lat_ll)/float(my_lat)
      TDX = lon_del
      TDY = lat_del
      
c     Open the terrain file for the Rhine Valley 
c     Longitude maps into X and Latitude maps into Y
c
      open(unit=8,file='copsTerrain.dat',status='old')
      print *,'copsTerrain.dat has been opened'

c     The first value in the table of terrain heights is at 
c     latitude=50, longitude=6.  The zgg array is filled 
c     from the maximum latitude (y) to the minimum starting
c     with latitude (y) = 50.  For each latitude row (j) value   
c     the longitude (x) direction is read in an implied (i) 
c     do-loop.
c     
c     Outer j-loop in the -y direction (decreasing Latitude)
c     Inner i-loop in the +x direction (increasing Longitude)
c     
      do j=my_lat,1,-1
         read(8,13)(zgg(i,j),i=1,mx_lon)
 13      format(7x,480f6.0)
      end do
      
c     Finished reading in the terrain heights.
c     
      do j=my_lat,1,-1
         latitude=50.0-float(j-1)*lat_del
c         write(6,31)latitude,j
c         write(7,31)latitude,j
 31      format(/,'New row - latitude=',f11.8,' j=',i3)
         do i=1,mx_lon
            longitude=6.0+float(i-1)*lon_del
c            write(6,33)longitude,i,zgg(i,j)
c            write(7,33)longitude,i,zgg(i,j)
 33         format('Longitude (column)=',f11.8,' i=',i3,
     X           ' zgg(i,j)=',f6.0)
         end do
      end do

c     Loop over the current (X,Y) grid, convert these to
c     lat/lon values, find the surrounding cells, and 
c     interpolate
c     
      mxy=nj
      mxx=ni
      xdel=xd
      ydel=yd
      print *,'mxx,mxy,xdel,ydel=',mxx,mxy,xdel,ydel
      print *,'Tx1,ty1,tdx,tdy=',tx1,ty1,tdx,tdy

      DO 100 J=1,MXY
         YP=YMIN+(J-1)*YDEL
         DO 90 I=1,MXX
            XP=XMIN+(I-1)*XDEL
            VEX(I,J)=BDVAL
            CALL XY2LLDRV(PLAT_OUT,PLON_OUT,XP,YP,ORLAT,
     X           ORLON,ANGXAX)
            X=PLON_OUT
            Y=PLAT_OUT
            IF((X.GE.TX1.AND.X.LE.TX2).AND.
     X           (Y.GE.TY1.AND.Y.LE.TY2))THEN
               IX=1.0001+(X-TX1)/TDX
               IY=1.0001+(Y-TY1)/TDY
               XE=TX1+IX*TDX
               XW=XE-TDX
               YN=TY1+IY*TDY
               YS=YN-TDY
               F1=ZGG(IX  ,IY  )
               F2=ZGG(IX+1,IY  )
               F3=ZGG(IX  ,IY+1)
               F4=ZGG(IX+1,IY+1)
               print *,'j,i=',j,i,' f1,f2,f3,f4=',f1,f2,f3,f4
               IF(F1.NE.BDVAL .AND. F2.NE.BDVAL .AND.
     X              F3.NE.BDVAL .AND. F4.NE.BDVAL)THEN
                  F12=FINT(XW,F1,XE,F2,X)
                  F34=FINT(XW,F3,XE,F4,X)
                  ZGRND=FINT(YS,F12,YN,F34,Y)
               END IF
            END IF
            IF(ZGRND.LE.EPS)THEN
               VEX(I,J)=FLOOR
            ELSE
               VEX(I,J)=ZGRND*0.001
            END IF
c            print *,'J=',j,' yp=',yp,' lat=',y
c            print *,'I=',i,' yp=',xp,' lon=',x
c            print *,'VEX=',vex(i,j)
 90      CONTINUE
 100  CONTINUE
      RETURN
      END
      
c
c----------------------------------------------------------------------X
c
      SUBROUTINE XY2LLDRV(PLAT_OUT,PLON_OUT,XP,YP,ORLAT_IN,ORLON_IN,
     X     ANGXAX)
C
C     DRIVER ROUTINE FOR COMPUTING LAT AND LONGITUDE FROM 
C     X,Y  COORDINATES RELATIVE TO LOCATION SPECIFIED BY ORLAT, ORLON.
C     THIS ROUTINE (AND XY2LL) WILL WORK FOR POINTS IN ANY PART OF
C     THE GLOBE WITH SOME RESTRICTIONS (SEE BELOW). THE ANGLE CONVENTIONS ARE:
C     0   < LAT < 90   ==>  NORTHERN HEMISPHERE
C     -90 < LAT < 0    ==>  SOUTHERN HEMISPHERE
C     
C     0    < LON < 180 ==>  WESTERN HEMISPHERE
C     -180 < LON < 0   ==>  EASTERN HEMISPHERE
C
C     PLAT  - OUTPUT LATITUDE OF POINT AT POSITION (X,Y)
C     PLON  - OUTPUT LONGITUDE OF POINT AT POSITION (X,Y)
C     XP,YP - INPUT COORDINATES IN KM OF LOCATION TO BE CONVERTED
C     ORLAT - INPUT LATIT. OF REFERENCE POINT (ORIGIN)
C     ORLON - INPUT LONG.  OF REFERENCE POINT (ORIGIN)
C     ANGXAX- ANGLE OF X-AXIS RELATIVE TO TRUE NORTH (USUALLY 90.0)
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
C     TO FIND THE LAT, LON OF A POINT AT (-1000. km, -1000. km). 
C     YOU COULD FIND THE LAT, LON OF A POINT AT (-1000. km, 1000. km), HOWEVER.
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

      X=XP
      Y=YP
C
C     ROTATE, IF NECESSARY
C
      IF (ANGXAX.NE.90.0) THEN
         THETA=(90.0-ANGXAX)*DEGRAD
         XT=X
         YT=Y
         X=XT*COS(THETA) - YT*SIN(THETA)
         Y=XT*SIN(THETA) + YT*COS(THETA)
      END IF


C
C     DETERMINE IF A HEMISPHERE BOUNDARY IS CROSSED
C
      ICROSS=0
      IF (ORLON.GT.0.0) THEN
         IF (X.GT.0.0 .AND. ORLON.LT.90.0) THEN
            CALL LL2XYDRV(ORLAT,EPS,X2,Y2,ORLAT,ORLON,90.0)
            IF (X2.LT.X) ICROSS=1
         ELSE IF (X.LT.0.0 .AND. ORLON.GT.90.0) THEN
            CALL LL2XYDRV(ORLAT,(180.-EPS),X2,Y2,ORLAT,ORLON,90.0)
            IF (X2.GT.X) ICROSS=2
         END IF
      ELSE IF (ORLON.LT.0.0) THEN
         IF (X.GT.0.0 .AND. ORLON.LT.-90.0) THEN
            CALL LL2XYDRV(ORLAT,(EPS-180.0),X2,Y2,ORLAT,ORLON,90.0)
            IF (X2.LT.X) ICROSS=3
         ELSE IF (X.LT.0.0 .AND. ORLON.GT.-90.0) THEN
            CALL LL2XYDRV(ORLAT,-EPS,X2,Y2,ORLAT,ORLON,90.0)
            IF (X2.GT.X) ICROSS=4
         END IF
      END IF

      IF (ORLAT.GT.0.0) THEN
         IF (Y.GT.0.0) THEN
            CALL LL2XYDRV((90.0-EPS),ORLON,X3,Y3,ORLAT,ORLON,90.0)
            IF (Y3.LT.Y) THEN
               WRITE(*,10)
 10            FORMAT(/,5X,'+++XY2LL ROUTINE CANNOT HANDLE CASE',
     X              ' WHERE POINTS CROSS A POLE+++')
c               CALL FLUSH_STDOUT
            END IF
         ELSE
            CALL LL2XYDRV(EPS,ORLON,X3,Y3,ORLAT,ORLON,90.0)
            IF (Y3.GT.Y) THEN
               IF (ICROSS.EQ.0) THEN
                  ICROSS=5
               ELSE
                  WRITE(*,20)
 20               FORMAT(/,5X,'+++ XY2LL CANNOT HANDLE DUAL HEMISPHERE',
     X                 ' CROSSOVER ','+++')
c                  CALL FLUSH_STDOUT
               END IF
            END IF
         END IF
      ELSE IF (ORLAT.LT.0.0) THEN
         IF (Y.GT.0.0) THEN
            CALL LL2XYDRV(-EPS,ORLON,X3,Y3,ORLAT,ORLON,90.0)
            IF (Y3.LT.Y) THEN
               IF (ICROSS.EQ.0) THEN
                  ICROSS=6
               ELSE
                  WRITE(*,20)
c                  CALL FLUSH_STDOUT
               END IF
            END IF
         ELSE IF (Y.LT.0.0) THEN
            CALL LL2XYDRV((EPS-90.0),ORLON,X3,Y3,ORLAT,ORLON,90.0)
            IF (Y3.GT.Y) THEN
               WRITE(*,10)
c               CALL FLUSH_STDOUT
            END IF
         END IF
      END IF

C
C     NOW PERFORM THE CALCULATIONS
C
      IF (ICROSS.EQ.0) THEN
C
C     NO CROSSOVER
C
         IF (ORLON.LT.0.0) THEN
            ORLON2=-ORLON
            X2=-X
         ELSE
            ORLON2=ORLON
            X2=X
         END IF
         IF (ORLAT.LT.0.0) THEN
            ORLAT2=-ORLAT
            Y2=-Y
         ELSE
            ORLAT2=ORLAT
            Y2=Y
         END IF
         CALL XY2LL(PLAT,PLON,X2,Y2,ORLAT2,ORLON2)
         IF (ORLON.LT.0.0) PLON=-PLON
         IF (ORLAT.LT.0.0) PLAT=-PLAT
      ELSE IF (ICROSS.EQ.1) THEN
C
C     +LON (EPS) -> -LON (-EPS)
C
         XNEW=X-X2
         YNEW=Y-Y2

         IF (ORLAT.LT.0.0) THEN
            ORLAT2=-ORLAT
            Y2=-YNEW
         ELSE
            ORLAT2=ORLAT
            Y2=YNEW
         END IF
         ORLON2=EPS
         X2=-XNEW

         CALL XY2LL(PLAT,PLON,X2,Y2,ORLAT2,ORLON2)
         PLON=-PLON
         IF (ORLAT.LT.0.0) PLAT=-PLAT
      ELSE IF (ICROSS.EQ.2) THEN
C
C     +LON (180-EPS) -> -LON (EPS-180)
C
         XNEW=X-X2
         YNEW=Y-Y2

         IF (ORLAT.LT.0.0) THEN
            ORLAT2=-ORLAT
            Y2=-YNEW
         ELSE
            ORLAT2=ORLAT
            Y2=YNEW
         END IF
         ORLON2=180.0-EPS
         X2=-XNEW
         
         CALL XY2LL(PLAT,PLON,X2,Y2,ORLAT2,ORLON2)
         PLON=-PLON
         IF (ORLAT.LT.0.0) PLAT=-PLAT
      ELSE IF (ICROSS.EQ.3) THEN
C
C     -LON (EPS-180) -> +LON (180-EPS)
C
         XNEW=X-X2
         YNEW=Y-Y2

         IF (ORLAT.LT.0.0) THEN
            ORLAT2=-ORLAT
            Y2=-YNEW
         ELSE
            ORLAT2=ORLAT
            Y2=YNEW
         END IF
         ORLON2=180.0-EPS
         X2=XNEW

         CALL XY2LL(PLAT,PLON,X2,Y2,ORLAT2,ORLON2)
         IF (ORLAT.LT.0.0) PLAT=-PLAT
      ELSE IF (ICROSS.EQ.4) THEN
C
C     -LON (-EPS) -> +LON (EPS)
C
         XNEW=X-X2
         YNEW=Y-Y2

         IF (ORLAT.LT.0.0) THEN
            ORLAT2=-ORLAT
            Y2=-YNEW
         ELSE
            ORLAT2=ORLAT
            Y2=YNEW
         END IF
         ORLON2=EPS
         X2=XNEW

         CALL XY2LL(PLAT,PLON,X2,Y2,ORLAT2,ORLON2)
         IF (ORLAT.LT.0.0) PLAT=-PLAT
      ELSE IF (ICROSS.EQ.5) THEN
C
C     +LAT -> -LAT
C
         XNEW=X-X3
         YNEW=Y-Y3

         IF (ORLON.LT.0.0) THEN
            ORLON2=-ORLON
            X3=-XNEW
         ELSE
            ORLON2=ORLON
            X3=XNEW
         END IF
         ORLAT2=EPS
         Y3=-YNEW

         CALL XY2LL(PLAT,PLON,X3,Y3,ORLAT2,ORLON2)
         PLAT=-PLAT
         IF (ORLON.LT.0.0) PLON=-PLON
      ELSE IF (ICROSS.EQ.6) THEN
C
C     -LAT -> +LAT
C
         XNEW=X-X3
         YNEW=Y-Y3

         IF (ORLON.LT.0.0) THEN
            ORLON2=-ORLON
            X3=-XNEW
         ELSE
            ORLON2=ORLON
            X3=XNEW
         END IF
         ORLAT2=EPS
         Y3=YNEW

         CALL XY2LL(PLAT,PLON,X3,Y3,ORLAT2,ORLON2)
         IF (ORLON.LT.0.0) PLON=-PLON
      END IF
C
C     Return PLAT_OUT, PLON_OUT with conventional sign convention [North (+), 
C     West (-)] rather than the one imposed by the LATLON command [North (+), 
C     West (+)].
C
      PLAT_OUT =  LAT_SIGN*ABS(PLAT)
      PLON_OUT = -LON_SIGN*ABS(PLON)

      RETURN

      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE XY2LL (DEGLAT,DEGLON,X,Y,SWLAT,SWLON)
C
C-----COMPUTES LATITUDE AND LONGITUDE FROM X,Y COORDINATES RELATIVE TO
C     LOCATION SPECIFIED BY SWLAT,SWLON.
C
C     DEGLAT- OUTPUT PARAMETER IN DEGREES OF LATITUDE
C     DEGLON- OUTPUT PARAMETER IN DEGREES OF LONGITUDE
C     X,Y-    INPUT COORDINATES OF LOCATION TO BE CONVERTED
C     SWLAT-  INPUT PARAMETER IN DEG OF LATITUDE OF REFERENCE LOCATION
C     SWLON-  INPUT PARAMETER IN DEG OF LONG. OF REFERERENCE LOCATION
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL DEGLAT,DEGLON,X,Y,SWLAT,SWLON

C      REAL  ACZ
C      REAL  CANG
C      REAL  DTR
      DOUBLE PRECISION  LAMDA1,LAMDA2
C      REAL  PHI1,PHI2
C      REAL  R
C      REAL  RTD
C      REAL  SANG
C      REAL  THETA
C
      DATA DEGARC/111.354/
      DATA DTR,RTD/0.017453293,57.2957795/
C
      DEGLAT = SWLAT
      DEGLON = SWLON
C
      R = SQRT(X*X + Y*Y)
      IF (R .LT. 0.01) GO TO 10
C
      THETA = ATAN2(X,Y)
      R = (R/DEGARC) * DTR
C
      PHI1 = DTR * SWLAT
      LAMDA1 = DTR * SWLON
C
      SANG = COS(THETA)*COS(PHI1)*SIN(R)+SIN(PHI1)*COS(R)
      IF(ABS(SANG).GT.1.0) THEN
         IF(SANG .LT. 0.0) SANG = -1.0
         IF(SANG .GT. 0.0) SANG =  1.0
      ENDIF
CSANG=SIGN(1.0,SANG)
      PHI2=ASIN(SANG)
C
      CANG = (COS(R)-SIN(PHI1)*SIN(PHI2))/(COS(PHI1)*COS(PHI2))
      IF(ABS(CANG).GT.1.0) THEN
         IF(CANG .LT. 0.0) CANG = -1.0
         IF(CANG .GT. 0.0) CANG =  1.0
      ENDIF
CCANG=SIGN(1.0,CANG)
      ACZ=ACOS(CANG)
C
      IF (X .LT. 0.0) THEN
          LAMDA2 = LAMDA1 + ACZ
      ELSE
          LAMDA2 = LAMDA1 - ACZ
      END IF
C
      DEGLAT = RTD * PHI2
      DEGLON = RTD * LAMDA2
C
 10   CONTINUE
C
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
c            CALL FLUSH_STDOUT
         ELSE
            ICROSS=3
         END IF
      ELSE IF (SIGN(1.0,ORLON).LT.SIGN(1.0,PLON)) THEN
         IF (ICROSS.NE.0) THEN
            WRITE(*,10)
c            CALL FLUSH_STDOUT
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
