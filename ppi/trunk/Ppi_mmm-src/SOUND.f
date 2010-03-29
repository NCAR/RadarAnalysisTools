c
c----------------------------------------------------------------------X
c
      SUBROUTINE SOUND(IOUT,NIN1,NGRD,X0,Y0,H0,C1,C2,C3,C4,ANGXAX)
C
C  FUNCTION - SOUND: F(OUT)=Sounding field mapped to radar space
C
C     IOUT    - Output field number
C     NIN1    - Name of desired sounding field for mapping to radar space
C     NGRD    - Generate NIN1 at regular (NGRD='REG') or
C               radar sampling locations (NGRD='   ')
C     ANGXAX  - Azimuth angle of +X axis
C     AZA,ELA - AZIMUTH AND ELEVATION ANGLES OF RADAR BEAMS
C     ITPOLD  - SCANNING MODE [RHI (3): AZA CONTAINS ELEVATION ANGLE;
C                                       ELA     "     AZIMUTH    "  ]
C     X0,Y0,HO  - (X,Y,Z) COORDINATES FOR  THIS   RADAR
C     C1,C2,C3  -    "         "       "  ANOTHER   "
C     C4        - FLAG: (0) C1,C2,C3 ARE FOR  THIS   RADAR
C                       (1)     "     "   "  ANOTHER   "
C-------*-------*-------*-------*-------*-------*-------*-------*-------
C       SOUND   Vrad    RADVEL          X0      Y0      Z0      0.0
C       SOUND   Vrad    RADVEL                                  0.0
C       SOUND   Temp    TEMP
C       SOUND   Uenv    U
C       SOUND   Venv    V
C-------*-------*-------*-------*-------*-------*-------*-------*-------
C

      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      INCLUDE 'snding.inc'
c      PARAMETER (MXFUN=10,KMX=10,MXW=255)
      PARAMETER (MXFUN=14)
      CHARACTER*8 LFUN(MXFUN),NIN1
      CHARACTER*3 NGRD
      DATA RE,REI/17000.0,1.17647E-04/
      DATA TORAD,TODEG/0.017453293,57.29577951/
      DATA PI,PI2,PI4/3.141592654,6.283185307,12.56637061/
      DATA EPS/0.001/

C     MGLASS SOUNDING INFORMATION (see snding.inc):
C
C     (XSND,YSND,ZSND) - (X,Y,Z) position (KM), Z in Meters ('M'), 
C                        Decameters ('D'), or Kilometers (' ')
C     (USND,VSND,WSND) - Winds (m/s)
C     (SSND,DSND)      - Wind speed (knots) and direction (deg)
C     (PSND,TSND)      - Pressure and air temperature
C     (DPSND,RHSND)    - Dew point temperature and relative humidity
C     ISND             - Number of sounding levels (ISND.le.MXSND)
C
C     Input cloud base conditions ==> adiabatic values:
C        BASEP, BASET - Cloud base p (mb), T(deg C)
C        AOS    - equivalent potential temperature (saturated adiabat)
C        AOD    - potential temperature (dry adiabat)
C        TCLD   - temperature along saturated (dry) adiabat through 
C                 cld base to represent in-cloud (sub-cloud) temperature
C        DTEMP  - temperature difference (tcloud - tair)
C        CLDMIX - Adiabatic cloud-water mixing ratio (g/kg)
C        CLDLWC -     "     cloud-water content
C
      DATA LFUN/'PRES    ','TEMP    ','DEWPT   ',
     X          'RH      ','U       ','V       ',
     X          'SPD     ','DIR     ','ASCENT  ',
     X          'RADVEL  ','TCLOUD  ','DTEMP   ',
     X          'CLDMIX  ','CLDLWC  '/

C     Find the index (IFUN) of the function (NIN1)
C     within a long list (LFUN) of length MXFUN.
C
      IFUN=IFIND(NIN1,LFUN,MXFUN)
      IF(IFUN.EQ.0)THEN
         PRINT *,'*** WARNING - UNKNOWN FIELD ***'
         RETURN
      END IF

C     Note: If ISW=1, coordinates only at original sampling locations
C              ISW=2, coordinates at regular range-angle grid
C     RHI (3) - AZA contains elevation angle and FXOLD = azimuth
C     Others  - AZA contains azimuth   angle and FXOLD = elevation
C
      IF(NGRD.EQ.'REG')THEN
         ISW=2
      ELSE
         ISW=1
      END IF

C     C4: Map to coordinates from (0) this radar, or (1) another radar.
C
      IF(C4.EQ.1.0)THEN
         XRAD = C1
         YRAD = C2
         HRAD = C3
      ELSE
         XRAD = X0
         YRAD = Y0
         HRAD = H0
      END IF

c      print *,'SOUND: isw,xyz=',isw,xrad,yrad,hrad
c      print *,'     nin1,isnd=',nin1,isnd
c      print *,'   nang,itp,fx=',nang(isw),itpold,fxold
c      print *,'     mn,mxgate=',mngate,mxgate

      DO 100 J=1,NANG(ISW)
         
C     RHI scan:
C     
         IF(ITPOLD.EQ.3)THEN
            IF(ISW.EQ.2)THEN
               AZRAD=FXOLD-AZROT(ITPOLD)
            ELSE
               AZRAD=ELA(J,ISW)
            END IF
            ELRAD=AZA(J,ISW)
            
C     All other scans:
C     
         ELSE
            AZRAD=AZA(J,ISW)-AZROT(ITPOLD)
            IF(ISW.EQ.2)THEN
               ELRAD=FXOLD
            ELSE
               ELRAD=ELA(J,ISW)
            END IF
            IF(AZRAD.LT.0.0)AZRAD=AZRAD+360.0
         END IF
         SINA=SIN(AZRAD*TORAD)
         COSA=COS(AZRAD*TORAD)
         SINE=SIN(ELRAD*TORAD)
         COSE=COS(ELRAD*TORAD)
         
         DO 90 I=MNGATE,MXGATE
            DAT(I,J,IOUT)=BDVAL
            HRNG=RNG(I,ISW)*COSE
            X=XRAD+HRNG*SINA
            Y=YRAD+HRNG*COSA
            Z=HRAD+RNG(I,ISW)*SINE+0.5*HRNG*HRNG*REI
            
C           Compute weights for linear interpolation of sounding
C
C     F(Z) = F(k)*[Z(k+1)-Z]/[Z(k+1)-Z(k)] + F(k+1)*[Z-Z(k)]/[Z(k+1)-Z(k)]
C     F(Z) = F(k)*[ZT    -Z]/(ZT    -ZB)   + F(k+1)*[Z-ZB  ]/(ZT    -ZB)
C     F(Z) = FB  * WB                      + FT    * WT
C
            DO K = 1, ISND-1
               ZB = ZSND(K)
               ZT = ZSND(K+1)
               IF(Z.GE.ZB .AND. Z.LE.ZT)THEN
                  KB=K
                  KT=K+1
                  DENOM=ZT-ZB
                  WB = (ZT-Z)/DENOM
                  WT = (Z-ZB)/DENOM
                  GO TO 10
               END IF
            END DO
            GO TO 90
 10         CONTINUE

            GOTO (11,12,13,14,15,16,17,18,19,20,
     X            21,22,23,24),IFUN

C     Pressure (mb)
C
 11         FB=PSND(KB)
            FT=PSND(KT)
            GOTO 30

C     Air temperature (C)
C
 12         FB=TSND(KB)
            FT=TSND(KT)
            GOTO 30

C     Dew point temperature (C)
C
 13         FB=DPSND(KB)
            FT=DPSND(KT)
            GOTO 30

C     Relative humidity (%)
C
 14         FB=RHSND(KB)
            FT=RHSND(KT)
            GOTO 30

C     U wind component (m/s)
C
 15         FB=USND(KB)
            FT=USND(KT)
            GOTO 30

C     V wind component (m/s)
C
 16         FB=VSND(KB)
            FT=VSND(KT)
            GOTO 30

C     Wind speed (m/s)
C
 17         FB=SSND(KB)
            FT=SSND(KT)
            GOTO 30

C     Wind direction (deg)
C
 18         FB=DSND(KB)
            FT=DSND(KT)
            GOTO 30

C     Balloon ascent rate (m/s)
C
 19         FB=WSND(KB)
            FT=WSND(KT)
            GOTO 30

C     Radial velocity (m/s)
C
 20         FB=USND(KB)
            FT=USND(KT)
            UDAT=FB*WB+FT*WT
            FB=VSND(KB)
            FT=VSND(KT)
            VDAT=FB*WB+FT*WT
            DAT(I,J,IOUT)=COSE*(UDAT*SINA+VDAT*COSA)
            GOTO 90

C     Adiabatic temperature (deg C)
C
 21         FB=TCLD(KB)
            FT=TCLD(KT)
            GOTO 30

C     Temperature difference (TCLOUD - TAIR in deg C)
C
 22         FB=DTEMP(KB)
            FT=DTEMP(KT)
            GOTO 30

C     Adiabatic mixing ratio (g/kg)
C
 23         FB=CLDMIX(KB)
            FT=CLDMIX(KT)
            GOTO 30

C     Adiabatic cloud water content (g/m3)
C
 24         FB=CLDLWC(KB)
            FT=CLDLWC(KT)
            GOTO 30

 30         DAT(I,J,IOUT)=FB*WB+FT*WT
 90      CONTINUE
 100  CONTINUE

      RETURN
      END
