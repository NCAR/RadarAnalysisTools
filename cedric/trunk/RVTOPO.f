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
c          6         7         8         9        10
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
C
C     COMMON block variables returned from LAT_LON
C
      COMMON /HEMISPHERE/ LATSPHERE,LONSPHERE,LAT_SIGN,LON_SIGN
      CHARACTER*8 LATSPHERE,LONSPHERE
      REAL LAT_SIGN,LON_SIGN

      double precision latitude, longitude, del
      real lat_ll,lon_ll,lat_lr,lon_lr
      real lat_ul,lon_ul,lat_ur,lon_ur
      real lat_del,lon_del
      
C     Note:  Longitude maps into X-axis
C            Latitude maps into Y-axis
C
      data lon_ll,lat_ll/6.0,47.0/
      data lon_lr,lat_lr/10.0,47.0/
      data lon_ul,lat_ul/6.0,50.0/
      data lon_ur,lat_ur/10.0,50.0/


C     LINEAR INTERPOLATION FORMULA FOR FUNCTION AT (X1.LE.X.LE.X2)
C
C
      FINT(X1,F1,X2,F2,X)=F1+(F2-F1)*(X-X1)/(X2-X1)
      
C     Print out the input variables to RVTOPO
C
      print *,'RVTOPO: ',latlon,reflat,reflon,angxax
      print *,'RVTOPO: ',ni,nj,i1,i2,j1,j2
      print *,'RVTOPO: x=',CSP(1,1),CSP(2,1),CSP(3,1)
      print *,'RVTOPO: y=',CSP(1,2),CSP(2,2),CSP(3,2)
      print *,'RVTOPO: zip=',zip

      ORLAT = REFLAT
      ORLON = REFLON
      BDVAL = ZIP

      XMIN  = CSP(1,1)
      XMAX  = CSP(2,1)
      XD    = CSP(3,1)
      YMIN  = CSP(1,2)
      YMAX  = CSP(2,2)
      YD    = CSP(3,2)
      print *,'xmin,xmax,xd=',xmin,xmax,xd
      print *,'ymin,ymax,yd=',ymin,ymax,yd
      
c    6,50     10,50
c    ul------ur
c     |       |
c     |       |
c     |       |
c    ll------lr
c    6,47     10,47
c
      TX1 = lon_ll
      TX2 = lon_lr
      TY1 = lat_ll
      TY2 = lat_ul
      lon_del = (lon_lr-lon_ll)/float(mx_lon)
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
      close (unit = 8)
      print *,'Terrain file has been closed'
      
c     Finished reading in the terrain heights.
c     
      do j=1,my_lat
         latitude=lat_ll+float(j-1)*lat_del
         write(6,31)latitude,j
c         write(7,31)latitude,j
 31      format(/,'New row - latitude=',f11.8,' j=',i3)
         do i=1,mx_lon
            longitude=lon_ll+float(i-1)*lon_del

            write(6,33)longitude,i,zgg(i,j)
c            write(7,33)longitude,i,zgg(i,j)
 33         format('Longitude (column)=',f11.8,' i=',i3,
     X           ' zgg(i,j)=',f6.0)
         end do
      end do
      print *,'mx_lon,mx_lat=',mx_lon,my_lat
      print *,'lon_ll,lat_ll=',lon_ll,lat_ll
      print *,'lon_lr,lat_lr=',lon_lr,lat_lr
      print *,'lon_ul,lat_ul=',lon_ul,lat_ul
      print *,'lon_ur,lat_ur=',lon_ur,lat_ur
      write(6,41)lon_ll,lat_ll,zgg(1,1)
 41   format('Lon-lat ll, zgg(1,1)=',3f8.3)
      write(6,43)lon_lr,lat_lr,zgg(mx_lon,1)
 43   format('Lon-lat ll, zgg(1,mx_lon)=',3f8.3)
      write(6,45)lon_ul,lat_ul,zgg(1,my_lat)
 45   format('Lon-lat ul, zgg(1,mx_lat)=',3f8.3)
      write(6,47)lon_ur,lat_ur,zgg(mx_lon,my_lat)
 47   format('Lon-lat ul, zgg(mx_lon,my_lat)=',3f8.3)

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
      print *,'BEFORE LOOP TO CONVERT GRID XY to LAT-LON'
      print *,'LAT-LONSPHERE=',LATSPHERE,LONSPHERE
      print *,'LAT-LON SIGNS=',LAT_SIGN,LON_SIGN
      print *,'ORLAT,ORLON,ANGXAX=',orlat,orlon,angxax

      DO 100 J=1,MXY
         YP=YMIN+(J-1)*YDEL
         DO 90 I=1,MXX
            XP=XMIN+(I-1)*XDEL
            VEX(I,J)=BDVAL
            ZGRND=BDVAL
            CALL XY2LLDRV(PLAT_OUT,PLON_OUT,XP,YP,ORLAT,
     X           ORLON,ANGXAX)
c            CALL XY2LL(PLAT_OUT,PLON_OUT,XP,YP,ORLAT,
c     X           ORLON,ANGXAX)
            print *,'Grid XP,YP=',xp,yp
            print *,'Grid LA,LO=',plat_out,plon_out 
            X=PLON_OUT
            Y=PLAT_OUT
            IF((X.GE.TX1.AND.X.LE.TX2).AND.
     X         (Y.GE.TY1.AND.Y.LE.TY2))THEN
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
c               print *,'j,i=',j,i,' f1,f2,f3,f4=',f1,f2,f3,f4
               IF(F1.NE.BDVAL .AND. F2.NE.BDVAL .AND.
     X              F3.NE.BDVAL .AND. F4.NE.BDVAL)THEN
                  F12=FINT(XW,F1,XE,F2,X)
                  F34=FINT(XW,F3,XE,F4,X)
                  ZGRND=FINT(YS,F12,YN,F34,Y)
c                  ZGRND=F1
                  VEX(I,J)=ZGRND*0.001
               END IF
               VEX(I,J)=F1*0.001
            END IF
            write(6,81)J,I,YP,XP,Y,X
81          format(' J,I=',2I5,' YP,XP=',2f10.3, ' LatLon=',2f10.3)
c            print *,'J=',j,' yp=',yp,' lat=',y
c            print *,'I=',i,' xp=',xp,' lon=',x
            print *,'ZGRND,VEX=',zgrnd,vex(i,j)
 90      CONTINUE
 100  CONTINUE
      RETURN
      END
