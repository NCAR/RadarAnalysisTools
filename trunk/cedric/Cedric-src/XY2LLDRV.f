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
               CALL FLUSH_STDOUT
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
                  CALL FLUSH_STDOUT
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
                  CALL FLUSH_STDOUT
               END IF
            END IF
         ELSE IF (Y.LT.0.0) THEN
            CALL LL2XYDRV((EPS-90.0),ORLON,X3,Y3,ORLAT,ORLON,90.0)
            IF (Y3.GT.Y) THEN
               WRITE(*,10)
               CALL FLUSH_STDOUT
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
