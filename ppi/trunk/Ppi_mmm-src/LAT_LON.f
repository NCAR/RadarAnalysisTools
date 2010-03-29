c
c----------------------------------------------------------------------X
c
      SUBROUTINE LAT_LON(INDAT,LATSPHERE,LONSPHERE,LAT_SIGN,LON_SIGN,
     X     ORGLAT,ORGLON,ANGXAX)
C
C  SET HEMISPHERES FOR LATITUDE AND LONGITUDE 
C     AND (LATITUDE, LONGITUDE) FOR THE ORIGIN.
C     Forces internal sign convention used in XY2LL and LL2XY.
C     The default is Northern latitude and Western longitude
C
C     LATSPHERE - (NORTH) Northern hemisphere, latitude is positive.
C                 (SOUTH) Southern hemisphere, latitude is negative.
C                   0 < LAT < 90   ==>  NORTHERN HEMISPHERE
C                 -90 < LAT < 0    ==>  SOUTHERN HEMISPHERE
C
C     LONSPHERE - (WEST)  Western  hemisphere, longitude is positive.
C                 (EAST)  Eastern  hemisphere, longitude is negative.
C                    0 < LON < 180 ==>  WESTERN HEMISPHERE
C                 -180 < LON < 0   ==>  EASTERN HEMISPHERE
C     ORG_NAM   - If ORG_NAM(1:1) .EQ. 'K', then look up NEXRAD name 
C                 for origin latitude/longitude.
C
      CHARACTER*8 INDAT(10),LATSPHERE,LONSPHERE,ORG_NAM
      CHARACTER*4 NMORG
      REAL LAT_SIGN,LON_SIGN
      REAL LAT_DEG,LAT_MIN,LAT_SEC
      REAL LON_DEG,LON_MIN,LON_SEC

      CHARACTER*8 ANGXINP,BLANK
      DATA BLANK/'        '/

      READ(INDAT,10)LATSPHERE(1:4),LONSPHERE(1:4),ORG_NAM,LAT_DEG,
     X     LAT_MIN,LAT_SEC,LON_DEG,LON_MIN,LON_SEC,ANGXINP
 10   FORMAT(/A4,A4/A8/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/A8)

C     Set signs for latitude and longitude hemispheres
C
      IF(LATSPHERE(1:1) .EQ. 'S') THEN
         LATSPHERE= 'SOUTH'
         LAT_SIGN = -1.0
      ELSE
         LATSPHERE= 'NORTH'
         LAT_SIGN = +1.0
      END IF
      IF(LONSPHERE(1:1) .EQ. 'E') THEN
         LONSPHERE= 'EAST'      
         LON_SIGN = -1.0
      ELSE
         LONSPHERE= 'WEST'      
         LON_SIGN = +1.0
      END IF

C     Set origin latitude and longitude in degrees
C
      IF(LAT_DEG.NE.0.0)THEN
         IF(LAT_MIN.EQ.0.0 .AND. LAT_SEC.EQ.0.0)THEN
            ORGLAT = LAT_DEG
         ELSE
            ORGLAT = LAT_DEG + LAT_MIN/60.0 + LAT_SEC/3600.0
         END IF
      END IF
      IF(LON_DEG.NE.0.0)THEN
         IF(LON_MIN.EQ.0.0 .AND. LON_SEC.EQ.0.0)THEN
            ORGLON = LON_DEG
         ELSE
            ORGLON = LON_DEG + LON_MIN/60.0 + LON_SEC/3600.0
         END IF
      END IF
      IF(ANGXINP.NE.BLANK)THEN
         READ(ANGXINP,11)ANGXAX
 11      FORMAT(F8.0)
      ELSE
         ANGXAX=90.0
      END IF

C     Look up NEXRAD lat-lon to be used as origin
C
      NMORG = ORG_NAM(1:4)
      IF(NMORG(1:1).EQ.'K')THEN
         CALL GET_RADAR_LOCATION(ORGLAT,ORGLON,ORGALT,NMORG)
      END IF
      ORGLAT=ABS(ORGLAT)
      ORGLON=ABS(ORGLON)
            
 180  PRINT 200
 200  FORMAT(5X,'SUMMARY OF LATLON COMMAND')
      PRINT 201
 201  FORMAT(5X,'------- -- ------ -------')
      PRINT 202,LATSPHERE(1:5),'ern',LAT_SIGN,
     X          LONSPHERE(1:4),'ern',LON_SIGN,
     X          ORG_NAM,ORGLAT,ORGLON,ANGXAX
 202  FORMAT( 20X,' LATITUDE and SIGN CONVENTION : ',A5,A3,F5.1,
     X       /20X,'LONGITUDE and SIGN CONVENTION : ',A4,A3,1X,F5.1,
     X       /20X,'          ORIGIN NAME (NEXRAD): ',A8,
     X       /20X,'              ORIGIN LATITUDE : ',F10.4,
     X       /20X,'              ORIGIN LONGITUDE: ',F10.4,
     X       /20X,'              AZIMUTH +X AXIS : ',F10.1,/)
      RETURN
      END






