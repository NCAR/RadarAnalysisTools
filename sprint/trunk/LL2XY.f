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
