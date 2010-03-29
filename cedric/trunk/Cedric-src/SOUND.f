c
c----------------------------------------------------------------------X
c
      SUBROUTINE SOUND(IFUN,AZ,EL,ATR,Z,BDVAL,FZ)
C
C  FUNCTION - SOUND: F(OUT)=Sounding field mapped to radar space
C
C     IFUN  - Index of desired sounding field for mapping to radar space
C     AZ,EL - Azimuth and elevation angles of current XY-grid location
C     Z     - height of current XY-grid location
C     BDVAL - bad data value
C     FZ    - returned sounding value
C
C     MGLASS SOUNDING INFORMATION (see snding.inc):
C
C     (XSND,YSND,ZSND) - (X,Y,Z) position (KM), Z in Meters ('M'), 
C                        Decameters ('D'), or Kilometers (' ')
C     (USND,VSND,WSND) - Winds (m/s)
C     (SSND,DSND)      - Wind speed (knots) and direction (deg)
C     (PSND,TSND)      - Pressure and air temperature
C     (DPSND,RHSND)    - Dew point temperature and relative humidity
C     (HSND)           - Geopotential height (m)
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
      INCLUDE 'snding.inc'

C     Compute weights for linear interpolation of sounding
C     
C     F(Z) = F(k)*[Z(k+1)-Z]/[Z(k+1)-Z(k)] + F(k+1)*[Z-Z(k)]/[Z(k+1)-Z(k)]
C     F(Z) = F(k)*[ZT    -Z]/(ZT    -ZB)   + F(k+1)*[Z-ZB  ]/(ZT    -ZB)
C     F(Z) = FB  * WB                      + FT    * WT
C     
C     Find indices for sounding values below (KB) and above (KT) 
C     current Z-level, along with interpolation weights (WB,WT).
C
      ZB = ZSND(1)
      ZT = ZSND(ISND)
      IF(Z.LT.ZB .OR. Z.GT.ZT)THEN
         FZ=BDVAL
         RETURN
      END IF

      DO K = 1, ISND-1
         ZB = ZSND(K)
         ZT = ZSND(K+1)
         IF(Z.GE.ZB .AND. Z.LE.ZT)THEN
            KB=K
            KT=K+1
            DENOM=ZT-ZB
            WB = (ZT-Z)/DENOM
            WT = (Z-ZB)/DENOM
         END IF
      END DO

 10   CONTINUE

      GOTO (11,12,13,14,15,16,17,18,19,20,
     X     21,22,23,24),IFUN
      
C     Pressure (mb)
C     
 11   FB=PSND(KB)
      FT=PSND(KT)
      GOTO 30
      
C     Air temperature (C)
C     
 12   FB=TSND(KB)
      FT=TSND(KT)
      GOTO 30
      
C     Dew point temperature (C)
C     
 13   FB=DPSND(KB)
      FT=DPSND(KT)
      GOTO 30
      
C     Relative humidity (%)
C     
 14   FB=RHSND(KB)
      FT=RHSND(KT)
      GOTO 30
      
C     U wind component (m/s)
C     
 15   FB=USND(KB)
      FT=USND(KT)
      GOTO 30
      
C     V wind component (m/s)
C     
 16   FB=VSND(KB)
      FT=VSND(KT)
      GOTO 30
      
C     Wind speed (m/s)
C     
 17   FB=SSND(KB)
      FT=SSND(KT)
      GOTO 30
      
C     Wind direction (deg)
C     
 18   FB=DSND(KB)
      FT=DSND(KT)
      GOTO 30
      
C     Balloon ascent rate (m/s)
C     
 19   FB=WSND(KB)
      FT=WSND(KT)
      GOTO 30
      
C     Radial velocity (m/s)
C     
 20   FB=USND(KB)
      FT=USND(KT)
      UDAT=FB*WB+FT*WT
      FB=VSND(KB)
      FT=VSND(KT)
      VDAT=FB*WB+FT*WT
      SINA=SIN(AZ*ATR)
      COSA=COS(AZ*ATR)
      COSE=COS(EL*ATR)
      FZ=COSE*(UDAT*SINA+VDAT*COSA)
      GOTO 40
      
C     Adiabatic temperature (deg C)
C
 21   FB=TCLD(KB)
      FT=TCLD(KT)
      GOTO 30
      
C     Temperature difference (TCLOUD - TAIR in deg C)
C     
 22   FB=DTEMP(KB)
      FT=DTEMP(KT)
      GOTO 30
      
C     Adiabatic mixing ratio (g/kg)
C     
 23   FB=CLDMIX(KB)
      FT=CLDMIX(KT)
      GOTO 30
      
C     Adiabatic cloud water content (g/m3)
C     
 24   FB=CLDLWC(KB)
      FT=CLDLWC(KT)
      GOTO 30
      
 30   FZ=FB*WB+FT*WT

 40   RETURN
      END





