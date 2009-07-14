c
c----------------------------------------------------------------------X
c
      SUBROUTINE LAT_LON(KRD,LATSPHERE,LONSPHERE,LAT_SIGN,LON_SIGN)
C
C  SET HEMISPHERES FOR LATITUDE AND LONGITUDE 
C
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
C
C     Note: Stripped down from version in PPI_MMM and CEDRIC.  The ORIGIN
C           command is the preferred way to set radar and origin names in 
C           SPRINT.
C
      CHARACTER*8 KRD(10),LATSPHERE,LONSPHERE
      REAL LAT_SIGN,LON_SIGN

      READ(KRD,10)LATSPHERE,LONSPHERE
 10   FORMAT(/A8/A8)

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

 180  PRINT 200
 200  FORMAT(//5X,'SUMMARY OF LATLON COMMAND')
      PRINT 201
 201  FORMAT(5X,'------- -- ------ -------')
      PRINT 202,LATSPHERE(1:5),'ern',LAT_SIGN,
     X          LONSPHERE(1:4),'ern',LON_SIGN
 202  FORMAT(/20X,' LATITUDE and SIGN CONVENTION : ',A5,A3,F5.1,
     X       /20X,'LONGITUDE and SIGN CONVENTION : ',A4,A3,1X,F5.1,/)
      RETURN
      END






