      SUBROUTINE VADCORD(LEV,CONA,CONEXT,IFLAT,LATLON,
     X     HRNG,SRNG,AZM,ELEV,MXX,MXY)
C
C     Converts the regular XYE or XYZ grid to RAE for VAD analyses.
C     CONEXT - (1) +X-axis angle (deg), (2) Origin X (km), (3) Origin Y (km)
C     CONA   - (1) X-coord (km), (2) Y-coord (km), (3) Z-coord (km)
C     IFLAT  - (0) Curved earth, (1) Flat earth
C
C     DATA RE,REI/17000.0,1.17647E-04/
C     Height above a curved earth: Z=H0+SRNG*SINE+0.5*HRNG*HRNG*REI
C        where   H0   - height of the antenna feed
C              SRNG   - slant range
C              SINE   - sin(elevation angle)
C              HRNG   - horizontal range = SRNG*COSE
C              Rprime - 4/3 earth radius = 4/3 (6375) = 8500
C                       where Earth's radius is 6375 km from 
C                       Doviak and Zrnic, p. 15 Fig. 2.7
C              REI    - 1/Rprime = 1.17647E-04
C              RE     - 2*Rprime = 2*8500 = 17000
C
C     Normal cartesian:   (X,Y) represent distances in km
C     Longitude-latitude: (X,Y) represent degrees
C        X1=CSP(1,1)      X2=CSP(2,1)      XD=CSP(3,1)
C        Y1=CSP(1,2)      Y2=CSP(2,2)      YD=CSP(3,2)
C        Z1=CSP(1,3)      Z2=CSP(2,3)      ZD=CSP(3,3)
C
      INCLUDE 'CEDRIC.INC'
      PARAMETER (MAXCON=4) 
      PARAMETER (PI=3.141592654)
      PARAMETER (RAD_EARTH=6370.12)
      PARAMETER (DEGRAD=0.01745329)
      REAL LONVALN,LATVALN

      DIMENSION CONA(MAXCON),CONEXT(MAXCON)
      COMMON /VOLUME/ INPID(NID),ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),
     X                IRCP(NFMAX),MAPVID(NFMAX,2),CSP(3,3),NCX(3),
     X                NCXORD(3),NFL,NPLANE,BAD
      CHARACTER*2 NAMF

      COMMON /LEVELS/ VALLEV(MAXZLEV),VALNYQ(MAXZLEV)
      COMMON /AXUNTS/ IUNAXS,LABAXS(3,3),SCLAXS(3,3),AXNAM(3)
      CHARACTER*4 AXNAM
      COMMON /ORIGIN/ORIGIN_LAT,ORIGIN_LON,LATLON_KARD
      REAL ORIGIN_LAT,ORIGIN_LON
      REAL RDEG,RMIN,RSEC

      DIMENSION HRNG(MAXX,MAXY),SRNG(MAXX,MAXY)
      DIMENSION AZM(MAXX,MAXY),ELEV(MAXX,MAXY)

      DATA IR1,IR2/ 3, 11 /
      DATA EPS/0.005/
      DATA EPS2/1.0E-6/

C     LJM - May need to change ZSCALE to 100.0 for some SPRINT files
C           that used GRIDPPI and elevation angle > 32.768 deg.
C
      DATA ZSCALE/1000.0/
      LOGICAL LATLON

      ATR=ATAN(1.)/45.
      RTA=1./ATR
      C1=CONA(1)
      C2=CONA(2)
      C3=CONA(3)
      C4=CONA(4)
      print *,'VADCORD: cona   (1-4)=',c1,c2,c3,c4
      print *,'VADCORD: conext (1-3)=',conext(1),conext(2),conext(3)

      IBEG=1
      IEND=NCX(1)
      X1=CSP(1,1)
      DX=CSP(3,1)

      JBEG=1
      JEND=NCX(2)
      Y1=CSP(1,2)
      DY=CSP(3,2)

      KBEG=1
      KEND=NCX(3)
      Z1=CSP(1,3)
      DZ=CSP(3,3)

      K3=NCX(3)
      DCON=VALLEV(LEV)
      RCON=ATR*DCON
      TCON=TAN(RCON)
      ZRAD=0.001*ID(317)
C
C     CHECK FOR FLAT EARTH MODE AND SET ANGLES FOR +X AXIS (ANGR,ANGXAX)
C
      IF (IFLAT.EQ.1) THEN
         AKZI=0.0
      ELSE
         AKZI=1./17001.697
      END IF
      ANGR=AMOD((450.-CONEXT(1)),360.)*ATR
      ANGXAX=ID(40)/REAL(ID(69))

C-----GENERAL CALCULATION FOR RADAR GEOMETRY
C
      IF(LATLON) THEN
         REFLAT = ID(33)+(ID(34)/60.)+(ID(35)/(3600.*ID(68)))
         RDEG = ID(36)
         RMIN = ID(37)/60.
         RSEC = ID(38)/(3600.*ID(68))
         REFLON = ABS(RDEG) + ABS(RMIN) + ABS(RSEC)
      END IF
      print *,'VADCORD:     axes=',axnam
      print *,'            i,j,k=',ibeg,iend,jbeg,jend,kbeg,kend
      print *,'         k3,dx,dy=',k3,dx,dy
      print *,'      latlon,flat=',latlon,iflat
      print *,'     lat,lon,angx=',reflat,reflon,angxax

      ASNF=0.0
      ACSF=1.0
      XORTR=CONEXT(2)-C1
      YORTR=CONEXT(3)-C2
      Z=DCON

C     Outer loop over Y and Inner loop over X
C
      DO J=JBEG,JEND
         DO I=IBEG,IEND
            IF(LATLON) THEN
               LONVALN  = CSP(1,1) + (I-1)*CSP(3,1)
               LATVALN  = CSP(1,2) + (J-1)*CSP(3,2)
               CALL LL2XYDRV(LATVALN,LONVALN,XVAL,YVAL,REFLAT,
     X              REFLON,ANGXAX)
               X=XVAL
               Y=YVAL
               XT=ACSF*X-ASNF*Y
               YT=ASNF*X+ACSF*Y
            ELSE
               X=CSP(1,1)+(I-1)*CSP(3,1)
               Y=CSP(1,2)+(J-1)*CSP(3,2)
               XT=ACSF*X-ASNF*Y+XORTR
               YT=ASNF*X+ACSF*Y+YORTR
            END IF
            S2=XT*XT+YT*YT
            HT=SQRT(S2)
            IF(IFLAT.EQ.1)THEN
               ZCOR=0.0
            ELSE
               ZCOR=S2*AKZI
            END IF
            
            IF (AXNAM(3).EQ.'E')THEN
               ZRTR=HT*TCON-C3
            ELSE
               ZRTR=Z-C3
            ENDIF
            ZRTR=ZRTR-ZCOR
            ZRTRSQ=ZRTR*ZRTR
            
C     (X,Y) DISTANCE
            HRNG(I,J)=SQRT(XT*XT+YT*YT)
            
C     RANGE CALCULATION
            SRNG(I,J)=SQRT(XT*XT+YT*YT+ZRTRSQ)
            
C     AZIMUTH
            AZM(I,J)=0.0
            IF(XT.NE.0.0.OR.YT.NE.0.0)AZM(I,J)=ATAN2(XT,YT)*RTA
            IF(AZM(I,J).LT.0.0) AZM(I,J)=AZM(I,J)+360.0
            
C     ELEVATION
            RNGSQ=XT*XT+YT*YT+ZRTRSQ
            RNG=SQRT(RNGSQ)
            ZADJ=ZRTR +(ZRTRSQ-RNGSQ)*AKZI
            IF(RNG .GT. EPS) THEN
               ARC=ZADJ/RNG
               IF(ABS(ARC).LE.1.0) ELEV(I,J)=RTA*ASIN(ARC)
            ELSE
               ELEV(I,J) = 0.0
            END IF
c            if(mod(i,10).eq.0 .and. mod(j,10).eq.0)then
c               write(*,1770)latvaln,lonvaln,xt,yt,z,
c     +              hrng(i,j),srng(i,j),azm(i,j),elev(i,j)
c 1770          format('LL=',2f8.2,' xyz=',3f10.3,
c     +              ' hrae=',2f10.3,2f8.2)
c            end if
         END DO
      END DO
      
      RETURN
      END


