c
c----------------------------------------------------------------------X
c
      SUBROUTINE GETSND(KRD,OLAT,OLON,ANGXAX,ORLAT,ORLON,BAD)
C
C  Read in mobile GLASS sounding from latfile:
C     CTYP=LL, convert (PLAT,PLON) to (X,Y) distance from (OLAT,OLON)
C     CTYP=XY, convert (X,Y) distance to (PLAT,PLON)   "       "
C  Note: (OLAT,OLON) must be the (LAT,LON) of the plotting window origin,
C        where the window is defined by (GXMIN,GXMAX,GYMIN,GYMAX)
C     Filter the time series using 'UNIForm' OR 'TRIAngular' filter
C     with radius RTFILT.
C
C     TIME             - Input TIME (SEC or HMS)
C     (XINP,YINP)      - Input (X,Y) position, either LON/LAT ('LL') or 
C                        Kilometers ('XY')
C
C     MGLASS SOUNDING INFORMATION (see sound.inc):
C     (XSND,YSND,ZSND) - (X,Y,Z) position (KM), Z in Meters ('M'), 
C                        Decameters ('D'), or Kilometers (' ')
C     (USND,VSND)      - Horizontal Winds (m/s)
C     WSND             - Balloon ascent rate (m/s)
C     (SSND,DSND)      - Wind speed (knots) and direction (deg)
C     (PSND,TSND)      - Pressure and air temperature
C     (DPSND,RHSND)    - Dew point temperature and relative humidity
C     (HSND)           - Geopotential height (m)
C     ISND             - Number of sounding levels (ISND.le.MXSND)
C
C     Input cloud base conditions and calculate adiabatic values:
C        BASEP, BASET - Cloud base p (mb), T(deg C)
C        AOS    - equivalent potential temperature (saturated adiabat)
C        AOD    - potential temperature (dry adiabat)
C        TCLD   - temperature along saturated (dry) adiabat through 
C                 cld base to represent in-cloud (sub-cloud) temperature
C        DTEMP  - temperature difference (tcloud - tair)
C        CLDMIX - Adiabatic cloud-water mixing ratio (g/kg)
C        CLDLWC -     "     cloud-water content
C
C     SDFILT           - Filter type ('UNIF', 'TRIA')
C     RTFILT           - Filter radius (central +/- RTFILT points)
C     
C     Structure of MGLASS input file from NCAR/ATD
C        Time (sec)           - time from launch
C        Press (mb)           - pressure
C        Temp, Dewpt (C)      - air and dewpoint temperatures
C        RH (%)               - relative humidity
C        Uwind, Vwind (m/s)   - E-W, N-S wind components
C        Wspd, Dir (m/s, deg) - Wind speed and direction
C        dZ (m/s)             - Ascent rate
C        Lon, Lat (deg, deg)  - balloon position (longitude, latitude)
C        Rng, Az (km, deg)    - balloon position (range, azimuth)
C        Alt (m)              - balloon pressure altitude
C        Qp (mb)              - 
C        Qt (C)               - 
C        Qh (%)               -
C        Qu, Qv, Quv (m/s)    -
C
C          TIME           - time (GMT sec)
C          XINP,YINP,ZMSL - (x,y) km and height (m)
C          U,V,W          - (u,v,w) m/s
C          az,hdist       - azimuth and horizontal distance (km)
C          BASEP, BASET   - Cloud base p (mb), T(deg C)
C
      INCLUDE 'snding.inc'
      REAL MIXBASE
      integer winskip

      CHARACTER LATFILE*24,IDIR*4,SDFILT*8,TTYP*3,CTYP*3
      CHARACTER*8 KRD(10),JNDAT(10),BLANK
      character*80 mglass_line,title
      character*1 pltskewt
      character proj*5,lat*10,lon*11,year*4,mon*2,day*2,tyme*8

      DATA BLANK/'        '/
      DATA TODEG,ABSC/57.29577951,273.16/
      
      BADVAL=BAD
      READ(KRD,1)TTYP,CTYP,IDIR,OLAT,OLON,SDFILT,RTFILT,PLTSKEWT
 1    FORMAT(////A3,1X,A3,1X/A4,4X/F8.0/F8.0/A4,F4.0/A1)
      IF(OLAT.EQ.0.0 .AND. ORLAT.NE.0.0)OLAT=ORLAT
      IF(OLON.EQ.0.0 .AND. ORLON.NE.0.0)OLON=ORLON
      IF(IDIR.EQ.'WEST')THEN
         OLON=+1.0*ABS(OLON)
      ELSE IF(IDIR.EQ.'EAST')THEN
         OLON=-1.0*ABS(OLON)
      END IF
      IF(KRD(9)(1:4).EQ.BLANK)THEN
         SDFILT='NONE    '
      END IF
      LATFILE=KRD(2)//KRD(3)//KRD(4)
      WRITE(6,2)LATFILE,TTYP,CTYP,IDIR,OLAT,OLON,SDFILT(1:4),RTFILT,
     +     PLTSKEWT
 2    FORMAT(1X,'GETSND: SOUNDING FILE= ',A24,A3,1X,A3,1X,
     +       A4,4X,2F12.5,' Filter=',A4,F8.0, ' SkewT=',A1)
      OPEN(UNIT=9,FILE=LATFILE,STATUS='OLD')
c      READ(5,3)(JNDAT(I),I=1,10)
c 3    FORMAT(10A8)
      CALL KARDIN(KRD)
      IF(KRD(2).NE.'CLDBASE')THEN
         WRITE(6,4)
 4       FORMAT('*** GETSND MUST HAVE CLDBASE LINE',
     +        ' FOLLOWED BY END ***')
         STOP
      END IF
      READ(KRD,5)BASEP,BASET
 5    FORMAT(//F8.0/F8.0)
c      READ(5,3)(KRD(I),I=1,10)
      CALL KARDIN(KRD)
      IF(KRD(1).NE.'END     ')THEN
         WRITE(6,4)
         STOP
      END IF

C     Compute equivalent potential temperature (saturated adiabat)
C     and potential temperature (dry adiabat) through cloud base 
C     temperature (BASET) and pressure (BASEP).
C     Compute mixing ratio (MIXBASE g/kg) at cloud base
C
      BASET=BASET+ABSC
c      BASEP=BASEP*1000.0
      AOS=OS(BASET,BASEP)
      AOD=OD(BASET,BASEP)
c      TAIR=TDRY(AOD,BASEP) - ABSC
      MIXBASE=W(BASET,BASEP)
      BASET=BASET - ABSC

      WRITE(6,51)BASEP,BASET,AOS,MIXBASE,AOD
 51   FORMAT(1X,'CLOUD BASE CONDITIONS: P=',F6.1,' T=',F6.1,
     +     ' ThetaE=',F6.1, ' Mixing ratio=',F6.1,' Theta=',F6.1)

C     Read initial lines of header information
C
      do i=1,15
         read(9,7)mglass_line
 7       format(a80)
         print *,mglass_line
         if(i.eq.2)write(proj,71)mglass_line(36:40)
         if(i.eq.4)write(lon, 72)mglass_line(36:46)
         if(i.eq.4)write(lat, 73)mglass_line(49:58)
         if(i.eq.5)write(year,74)mglass_line(36:39)
         if(i.eq.5)write(mon, 75)mglass_line(42:43)
         if(i.eq.5)write(day, 76)mglass_line(46:47)
         if(i.eq.5)write(tyme,77)mglass_line(50:57)
 71      format(a5)
 72      format(a11)
 73      format(a10)
 74      format(a4)
 75      format(a2)
 76      format(a2)
 77      format(a8)
      end do
      write(title,78)proj,lat,lon,year,mon,day,tyme
 78   format(a5,' [',a10,' ',a11,'] ',a4,' ',a2,a2,'-',a8)
      print *,title
c      write(6,8)
c      write(6,9)
c 8    format(' Time  Press  Temp  Dewpt  RH    Uwind  Vwind  Wspd  Dir',
c     +     '   dZ      Lon     Lat    Rng   Az     Alt')
c 9    format('  sec    mb     C     C     %     m/s    m/s   m/s   deg',
c     +     '   m/s     deg     deg     km   deg     m')
         
      ISND=0
 10   READ(9,*,ERR=10,END=20)TIME,PRESS,TEMP,DEWPT,RH,U,V,SPD,DIR,
     X     WASCENT,YINP,XINP,RNG,AZ,ZMSL
c      write(6,11)time,press,temp,dewpt,rh,u,v,spd,dir,wascent,
c     x     yinp,xinp,rng,az,zmsl
c 11   format(f6.1,f7.1,3f6.1,2f7.1,3f6.1,f9.3,f8.3,2f6.1,f8.1)

      IF(CTYP(1:2).EQ.'LL')THEN
         PLAT=XINP
         IF(IDIR.EQ.'WEST')THEN
            PLON=+1.0*ABS(YINP)
         ELSE IF(IDIR.EQ.'EAST')THEN
            PLON=-1.0*ABS(YINP)
         END IF
         CALL LL2XYDRV(PLAT,PLON,X,Y,OLAT,OLON,ANGXAX)
      ELSE 
         X=XINP
         Y=YINP
         CALL XY2LLDRV(PLAT,PLON,X,Y,OLAT,OLON,ANGXAX)
      END IF

      ISND=ISND+1
      IF(ISND.GT.MXSND)THEN
         ISND=ISND-1
         GO TO 20
      END IF
      XSND(ISND)=X
      YSND(ISND)=Y
      IF(CTYP(3:3).EQ.'M')THEN
         ZSND(ISND)=ZMSL/1000.0
      ELSE IF (CTYP(3:3).EQ.'D')THEN
         ZSND(ISND)=ZMSL/100.0
      ELSE
         ZSND(ISND)=ZMSL
      END IF
      USND(ISND)=U
      VSND(ISND)=V
      WSND(ISND)=WASCENT

      SSND(ISND)=SPD
      DSND(ISND)=DIR
      PSND(ISND)=PRESS
      TSND(ISND)=TEMP
      DPSND(ISND)=DEWPT
      RHSND(ISND)=RH

C     Derive adiabatic values of temperature (TCLD), temperature difference
C     (DTEMP = TCLD - TAIR), mixing ratio (CLDMIX), and cloud liquid water 
C     content (CLDLWC).
C
      IF(PSND(ISND).GT.BASEP)THEN
         TCLD(ISND) = TDRY(AOD,PSND(ISND)) - ABSC
         DTEMP(ISND) = 0.0
         TKELVIN = BASET+ABSC
         CLDMIX(ISND) = W(TKELVIN,BASEP)
         CLDLWC(ISND)=0.0
      ELSE
         TCLD(ISND) = TSA(AOS,PSND(ISND)) - ABSC
         DTEMP(ISND) = TCLD(ISND) - TSND(ISND)
         TKELVIN = TCLD(ISND)+ABSC
         CLDMIX(ISND) = W(TKELVIN,PSND(ISND))
         CLDLWC(ISND)=(MIXBASE-CLDMIX(ISND))*0.34838*PSND(ISND)/TKELVIN
      END IF

C     Calculate horizontal range and azimuth of balloon 
C     relative to the origin (OLAT,OLON).
C
      XORIGIN=0.0
      YORIGIN=0.0
      XP=XSND(ISND)-XORIGIN
      YP=YSND(ISND)-YORIGIN
      HRNG=SQRT(XP*XP+YP*YP)
      IF(XP.EQ.0.0 .AND. YP.EQ.0.0)THEN
         AZIM=0.0
      ELSE IF(XP.GT.0.0 .AND. YP.EQ.0.0)THEN
         AZIM=90.0
      ELSE IF(XP.EQ.0.0 .AND. YP.LT.0.0)THEN
         AZIM=180.0
      ELSE IF(XP.LT.0.0 .AND. YP.EQ.0.0)THEN
         AZIM=270.0
      ELSE IF(XP.NE.0.0 .AND. YP.NE.0.0)THEN
         AZIM=TODEG*ATAN2(XP,YP)
      ELSE
         AZIM=0.0
      END IF
      IF(AZIM.LT.0.0)AZIM=AZIM+360.0

      TTYP='SEC'
      IF(TTYP.EQ.'SEC')THEN
         SEC=TIME
         IHR=SEC/3600.0
         IMN=(SEC-IHR*3600.0)/60.0
         ISC= SEC-IHR*3600.0-IMN*60.0
         IF(ISC.GE.60)THEN
            ISC=ISC-60
            IMN=IMN+1
         END IF
         IF(IMN.GE.60)THEN
            IMN=IMN-60
            IHR=IHR+1
         END IF
      ELSE
         IHR=TIME/10000.
         IMN=(TIME-IHR*10000.)/100.
         ISC=TIME-IHR*10000.-IMN*100.
         SEC=IHR*3600.+IMN*60.+FLOAT(ISC)
      END IF
      ISEC=INT(SEC)

c     Note: I2.2 causes leading zeros to be printed,
c           e.g. 13:01:05 rather than 13: 1: 5 for HH:MM:SS.
c
      IF(MOD(ISEC,60).EQ.0)THEN
c         WRITE(6,13)ISEC,XINP,YINP,X,Y,ZMSL,HRNG,AZIM,
c     X        PRESS,TEMP,DEWPT,RH,U,V,WASCENT
c 13      FORMAT('*T=',I5,' LL=',f8.3,f9.3,' XYZ=',2f7.2,f9.1,
c     +        ' HA=',2F6.1,' PTDR=',f7.1,3f6.1,' uvw=',3f7.1)
         WRITE(6,13)ISEC,XINP,YINP,X,Y,ZMSL,PRESS,TEMP,DEWPT,RH,U,V,
c     +        WASCENT
     +        TCLD(ISND),CLDMIX(ISND),CLDLWC(ISND)
 13      FORMAT('*T=',I4,' LL=',f8.3,f9.3,' xyz=',2f7.2,f9.1,
     +        ' ptdr=',f7.1,3f6.1,' uv=',2f5.1,' cld=',3f5.1)
      ELSE
         WRITE(6,15)ISEC,XINP,YINP,X,Y,ZMSL,PRESS,TEMP,DEWPT,RH,U,V,
c     +        WASCENT
     +        TCLD(ISND),CLDMIX(ISND),CLDLWC(ISND)
 15      FORMAT(' T=',I4,' LL=',f8.3,f9.3,' xyz=',2f7.2,f9.1,
     +        ' ptdr=',f7.1,3f6.1,' uv=',2f5.1,' cld=',3f5.1)
      END IF
      GO TO 10

 20   CONTINUE
      print *,'GETSND: isnd=',isnd
      IF(ISND.EQ.0)THEN
         WRITE(6,99)
 99      FORMAT(1X,'NO SOUNDING WAS READ')
         STOP
      END IF

      IF(SDFILT.NE.'NONE')THEN
         IRAD=INT(RTFILT)
         CALL SMTH1D(USND,SDFILT,IRAD,BADVAL,1,ISND,ISND)
         CALL SMTH1D(VSND,SDFILT,IRAD,BADVAL,1,ISND,ISND)
         CALL SMTH1D(WSND,SDFILT,IRAD,BADVAL,1,ISND,ISND)
         CALL SMTH1D(PSND,SDFILT,IRAD,BADVAL,1,ISND,ISND)
         CALL SMTH1D(TSND,SDFILT,IRAD,BADVAL,1,ISND,ISND)
         CALL SMTH1D(DPSND,SDFILT,IRAD,BADVAL,1,ISND,ISND)
         CALL SMTH1D(RHSND,SDFILT,IRAD,BADVAL,1,ISND,ISND)
         CALL SMTH1D(HSND,SDFILT,IRAD,BADVAL,1,ISND,ISND)
      END IF

C     CALL DRV_SKEWT does plotting of sounding
C     Routine WNDBARB (in skewt) converts (U,V) to SPD (knots)
C
      WINSKIP=5
      IF(PLTSKEWT.EQ.'Y')THEN
         CALL DRV_SKEW(PSND,TSND,DPSND,SSND,DSND,HSND,MXSND,
     X        ISND,TITLE,AOS,AOD,BASEP,BASET,MIXBASE,WINSKIP,
     X        BAD)
         CALL MYFRAME
      END IF

      CLOSE(9)
      RETURN
      END
c
c----------------------------------------------------------------------X
c
      subroutine drv_skew(psnd,tsnd,dpsnd,ssnd,dsnd,hsnd,mxsnd,
     x     isnd,title,aos,aod,basep,baset,mixbase,winskip,bad)
c
c From gill@ncar.UCAR.EDU Tue Nov 17 17:28:03 1992
c
c     ... how many values, here we make the assumption that
c         winds and temp are at the same level
c
      DIMENSION PSND(MXSND),TSND(MXSND),DPSND(MXSND),RHSND(MXSND)
      DIMENSION SSND(MXSND),DSND(MXSND),HSND(MXSND)
      parameter (mm=1000)
      dimension pres(mm),temp(mm),dwpt(mm),spd(mm),dir(mm)
      dimension tcld(mm),delt(mm),hght(mm)
      real lwc(mm),mixbase,mixrat(mm)
      COMMON /SKWDRW/ ISKDRW   
      character*80 title
      integer winskip
c
c---------------------------------------------------------------
c     pres - pressure (mb)
c     temp - temperature (C)
c     dwpt - dew point temperature (C)
C     basep, baset - Cloud base p (mb), T(deg C)
c     aos  - equivalent potential temperature (saturated adiabat)
c     aod  - potential temperature (dry adiabat)
c     tcld - temperature along saturated (dry) adiabat through 
c            cld base to represent in-cloud (sub-cloud) temperature
c     spd  - wind speed (m/s)
c     dir  - wind direction (degrees)
c     hght - geopotential height (m)
c
c     you can choose the value of a flag to mark suspect
      data absc/273.16/
c
c     wind speeds on the plot have the following interpretation
c        full pennant 50 kts
c        full barb    10 kts
c        half barb     5 kts
c---------------------------------------------------------------
c
c     ... flag that says activate the background, modified internally
c
      iskdrw=0

c     Transfer input arrays to plotting arrays
c
      print *,' '
      print *,'DRV-SKEWT:'
      WRITE(6,51)BASEP,BASET,AOS,MIXBASE,AOD
 51   FORMAT(1X,'CLOUD BASE CONDITIONS: P=',F6.1,' T=',F6.1,
     +     ' ThetaE=',F6.1, ' Mixing ratio=',F6.1,' Theta=',F6.1)
      badval=bad
      do i=1,mm
         pres(i)   = badval
         temp(i)   = badval
         dwpt(i)   = badval
         tcld(i)   = badval
         lwc(i)    = badval
         mixrat(i) = badval
         delt(i)   = badval
         spd(i)    = badval
         dir(i)    = badval
         hght(i)   = badval
      end do
      do i=1,isnd
         pres(i) = psnd(i)
         hght(i) = hsnd(i)
         temp(i) = tsnd(i)
         dwpt(i) = dpsnd(i)
         spd(i)  = ssnd(i)
         dir(i)  = dsnd(i)
         if(pres(i).eq.badval)go to 18
         if(temp(i).eq.badval)go to 18
         if(dwpt(i).eq.badval)go to 18
         if(mixbase.ne.badval)then
            if(pres(i).gt.basep)then
               tcld(i) = tdry(aod,pres(i)) - absc
               delt(i) = 0.0
               tkelvin = baset+absc
               mixrat(i) = w(tkelvin,basep)
               lwc(i)=0.0
            else
               tcld(i) = tsa(aos,pres(i)) - absc
               delt(i) = tcld(i) - temp(i)
               tkelvin = tcld(i)+absc
               mixrat(i) = w(tkelvin,pres(i))
               lwc(i)=(mixbase-mixrat(i))*0.34838*pres(i)/tkelvin
            end if
         end if

         if(pres(i).lt.500.0)then
            if(tcld(i).lt.temp(i))then
               tcld(i) = badval
               delt(i) = 0.0
            end if
         end if
         write(6,15)i,pres(i),hght(i),temp(i),dwpt(i),
     x        dir(i),spd(i),tcld(i),delt(i),mixrat(i),lwc(i)
 15      format('i=',i5,' p,H,T,Td,Dir,Spd=',6f7.1, 
     x        '   Tc,dT,Mix,Lwc=',4f7.1)
         k=i
         if(pres(i).lt.100.0)go to 20
 18      continue
      end do
 20   continue
      nn = k
c
c     open gks, shut off clipping, and allow for flashing
c
c-----CALL OPNGKS                                                   
      CALL GSCLIP (0)                                              
c-----CALL GOPWK (2,9,3)                                          
c
c     ... pass this routine the data, it does it all
c     
c-----print *,'Calling skewt; nn=',nn
      call skewt (pres,temp,dwpt,tcld,spd,dir,hght,nn,
     x     title,badval,winskip)
C                                                                       
c-----print *,'Back from skewt'
c
c     ... shut down GKS
c
c-----CALL GCLWK (2)  ! CLOSE GFLASH FOR MAP AND SKEW-T BACKGROUND       
c-----CALL CLSGKS                                                       
c
c     ... that's all folks
c
      return
      end
c
c----------------------------------------------------------------------X
c
      SUBROUTINE SKEWT (PRES,TEMP,DWPT,TCLD,SPD,DIR,HGHT,NN,
     X     TITLINE,BADVAL,WINSKIP)              
C                                                                       
C     SKEWT- PLOTS SOUNDINGS ON A SKEWT, LOG P THERMODYNAMIC DIAGRAM    
C                                                                       
C     PRES- PRESSURE ARRAY FOR THERMODYNAMIC DATA (MB)                  
C     TEMP- TEMPERATURE ARRAY (CELSIUS)                                 
C     DWPT- DEW POINT ARRAY (CELSIUS)                                   
C     TCLD- SATURATED ADIABAT ARRAY (CELSIUS)                                   
C     SPD- WIND SPEED ARRAY (M/S)                                       
C     DIR- WIND DIRECTION ARRAY (DEGREES-0 IS NORTH)                    
C     HGHT- Geopotential height (m)
C     NN- NUMBER OF DATA LEVELS                                         
C     BADVAL- VALUE ASSIGNED TO MISSING DATA  --TEMP,DWPT TRACES ARE    
C                   TERMINATED AS SOON AS THIS NUMBER IS ENCOUNTERED.   
C
C     OUTPUT PARAMETERS.....                                         
C     ALL INPUT PARAMETERS REMAIN UNCHANGED BY THIS ROUTINE.            
C                                                                       
      INTEGER WINSKIP
      DIMENSION PRES(1),TEMP(1),DWPT(1),TCLD(1),SPD(1),DIR(1),HGHT(1)
      CHARACTER LAB*120, ISTAT*80                                       
      CHARACTER*4 LABGEO
C                                                                       
      CHARACTER *80    TITLINE                                          
      INTEGER          ITSTRT,                                          
     *                 ITITLEN,                                         
     *                 LCNTR                                            
      COMMON /SKWDRW/ ISKDRW                                            
C                                                                       
C  DEGREES TO RADIANS                                                   
      PARAMETER (DTR = 0.0174532925)                                    
C  WIND BARB DATA                                                       
      PARAMETER (XM = 24.2)                                             
C                                                                       
C  LINE WIDTH VARIABLES                                                 
C                                                                       
      CHARACTER*2 ISPOPT
      DATA ISPOPT / 'LW' /                                              
      DATA ISWIDE / 1200 /                                              
C                                                                       
C  MAPPINGS FROM (P,T) TO CM ON SKEWT                                   
C                                                                       
      FY(P) = 132.182 - 44.061 * ALOG10(P)                              
      FX(T,Y) = 0.54 * T + 0.90692 * Y                                  
C                                                                       
C  TEST TO SEE IF A BACKGROUND HAS BEEN DRAWN, IF NOT CALL SKWTBKG      
C                                                                       
      IF (ISKDRW .EQ. 0) THEN                                           
        CALL SKWTBKG                                                    
        ISKDRW = 1                                                      
      END IF                                                            
C                                                                       
C  SKEWT BACKGROUND HAS BEEN GENERATED-- PLOT THE SOUNDING              
C                                                                       
      CALL GFLAS3(2)                                                   
      CALL SET(.05,.95,.05,.95,-19.0,27.1,-.9346217,44.061,1)           
C                                                                       
                                                                        
C  PUT ON TITLE                                                         
      print *,'SKEWT - Draw sounding'
                                                                        
      ITITLEN=LEN(TITLINE)                                           
c      CALL WTSTR (4.05,-2.4,TITLINE(1:ITITLEN),12,0,0)                  
      CALL WTSTR (3.175,-2.65,TITLINE(1:ITITLEN),12,0,0)
      IF(NN.GT.0) CALL LINE(XM,-.9346217,XM,44.061)                     
C                                                                       
C  SOLID DASH PATTERN, INCREASED SPOT SIZE (DEFAULT=8)                  
C                                                                       
      CALL DASHDB (65535)                                               
      CALL GETUSV (ISPOPT,ISNORM)                                       
      ISWIDE = ISWIDE + 3000                                            
      CALL SETUSV (ISPOPT,ISWIDE)                                       
      ISWIDE = ISWIDE - 3000                                            

c-----print *,'Starting plots; nn=',nn
      IF (NN.LE.0) GO TO 91

C     Plot air temperature vs. pressure
C
      CALL SETUSV('LW',3000)                                      
      DO 60 I=1,NN-1
         IF(PRES(I).EQ.BADVAL.OR.TEMP(I).EQ.BADVAL)GO TO 60
         IF(PRES(I+1).EQ.BADVAL.OR.TEMP(I+1).EQ.BADVAL)GO TO 60
         Y1=FY(PRES(I))                                                 
         X1=FX(TEMP(I),Y1)                                               
         Y2=FY(PRES(I+1))
         X2=FX(TEMP(I+1),Y2)
         CALL LINE(X1,Y1,X2,Y2)
   60 CONTINUE                                                          
c-----print *,'Finished p,T'

C     Plot dew point temperature vs. pressure
C
      CALL SETUSV('LW',1500)                                      
      DO 70 I=1,NN-1
         IF(PRES(I).EQ.BADVAL.OR.DWPT(I).EQ.BADVAL)GO TO 70                                  
         IF(PRES(I+1).EQ. BADVAL.OR.DWPT(I+1).EQ.BADVAL)GO TO 70
         Y1=FY(PRES(I))                                               
         X1=FX(DWPT(I),Y1)                                             
         Y2=FY(PRES(I+1))                                               
         X2=FX(DWPT(I+1),Y2)                                             
         CALL LINE(X1,Y1,X2,Y2)
   70 CONTINUE                                                          
c-----print *,'Finished p,Td'

C     Plot saturated adiabatic temperature vs. pressure
C
      CALL SETUSV('LW',3000)                                      
      JFLAG=0
      DO 80 I=1,NN-1
         IF(PRES(I).EQ. BADVAL.OR.TCLD(I).EQ.BADVAL)GO TO 80
         IF(PRES(I+1).EQ. BADVAL.OR.TCLD(I+1).EQ.BADVAL)GO TO 80
            Y1=FY(PRES(I))                                               
            X1=FX(TCLD(I),Y1)                                             
            Y2=FY(PRES(I+1))                                               
            X2=FX(TCLD(I+1),Y2)                                             
            CALL LINE(X1,Y1,X2,Y2)
   80 CONTINUE                                                          
c-----print *,'Finished p,Ts'

C     Plot wind vectors                                             
C                                                                       
      CALL SETUSV('LW',2000)                                      
      CALL SETUSV (ISPOPT,ISWIDE)                                       
      DO 85 I=1,NN,WINSKIP
         IF(PRES(I).EQ.BADVAL.OR.DIR(I).EQ.BADVAL)GO TO 85
c         IF (DIR(I) .GT. 360.) GO TO 85                                    
         ANG=DIR(I)*DTR                                                
         U = -SPD(I)*SIN(ANG)                                          
         V = -SPD(I)*COS(ANG)                                          
         Y1=FY(PRES(I))                                                
         CALL WNDBARB (XM,Y1,U,V)                                          
   85 CONTINUE                                                          
c-----print *,'Finished u,v'

C     Plot geopotential height scale
C                                                                       
      CALL SETUSV('LW',2000)                                      
      CALL SETUSV (ISPOPT,ISWIDE)                                       
      DO 90 I=1,NN
         IF(PRES(I).EQ.BADVAL.OR.HGHT(I).EQ.BADVAL)GO TO 90
         Y1=FY(PRES(I))                                                
         X1=XM+4.0
         IF(AMOD(PRES(I),50.0).EQ.0.0)THEN
            WRITE(LABGEO,87)INT(HGHT(I))
   87       FORMAT(I4)
            CALL PLCHLQ(X1,Y1,LABGEO,12.0,0.,0.)
         END IF
   90 CONTINUE                                                          

   91 CONTINUE                                                          
c-----print *,'Finished geopotential height scale'
                                                                       
C  RESET TO NORMAL SPOT SIZE AND EXIT                                   
C                                                                       
      CALL SETUSV (ISPOPT,ISNORM)                                       
      RETURN                                                            
      END                                                               
c
c----------------------------------------------------------------------X
c
      SUBROUTINE SKWTBKG                                                
C                                                                       
C  SKWTBKG- PLOTS BACKGROUND FOR A SKEWT, LOG P THERMODYNAMIC DIAGRAM   
C                                                                       
      COMMON /SKWDRW/ ISKDRW                                            
C                                                                       
C  ENCODE BUFFER                                                        
C                                                                       
      CHARACTER*4 ITIT                                                  
C                                                                       
C  SKEWT BORDER                                                         
C                                                                       
      DIMENSION XB(7),YB(7)                                             
      DATA XB/-19.,27.1,27.1,18.6,18.6,-19.,-19./                       
      DATA YB/-.9346217,-.9346217,9.,17.53,44.061,44.061,-.9346217/     
C                                                                       
C  PRESSURE LINE SPECS                                                  
C                                                                       
      DIMENSION PLV(11),PLN(11,2)                                       
      DATA PLV/100.,200.,300.,400.,500.,600.,700.,800.,900.,            
     &         1000.,1050./                                             
      DATA PLN/11*-19.,4*18.6,22.83,26.306,5*27.1/                      
C                                                                       
C  TEMPERATURE LINE SPECS                                               
C                                                                       
      DIMENSION TP(15,2)                                                
      DATA TP/8*1050.,855.,625.,459.,337.,247.,181.,132.,730.,580.,500.,
     &   430.,342.,251.,185.,135.,7*100./                               
C                                                                       
C  MIXING RATIO SPECS                                                   
C                                                                       
      REAL        RAT(8)                                                
      CHARACTER*2 LRAT(8)                                               
      DATA RAT/20.,12.,8.,5.,3.,2.,1.,0.4/                              
      DATA LRAT/'20','12',' 8',' 5',' 3',' 2',' 1','.4'/                
C                                                                       
C  DRY/SATURATED ADIABAT BUFFERS                                        
C                                                                       
      DIMENSION SX(162),SY(162),Y45(162)                                
C                                                                       
C  DEGREES TO RADIANS, ABSOLUTE ZERO                                    
C                                                                       
      PARAMETER (ABZ = 273.16)                                          
C                                                                       
C  MAPPINGS FROM (P,T) TO CM ON SKEWT                                   
C                                                                       
      FY(P)=132.182-44.061*ALOG10(P)                                    
      FX(T,Y)=0.54*T+0.90692*Y                                          
C                                                                       
C  DRAW SKEWT BORDER                                                    
C                                                                       
      CALL GFLAS1(2)                                                   
      CALL SET(.05,.95,.05,.95,-19.0,27.1,-.9346217,44.061,1)           
      CALL CURVE(XB,YB,7)                                               
C                                                                       
C  DRAW THE PRESSURE LINES                                              
C                                                                       
      DO 10 K=1,11                                                      
         Y1=FY(PLV(K))                                                  
         IF(K.NE.1.AND.K.NE.11) CALL LINE(PLN(K,1),Y1,PLN(K,2),Y1)      
         ITS=NINT(PLV(K))                                               
      WRITE (ITIT,101) ITS                                              
  101    FORMAT(I4)                                                     
C        CALL PWRY(-20.9,Y1,ITIT,4,1.9,0,0)                             
      CALL WTSTR (-19.2,Y1,ITIT,11,0,1)                                 
   10 CONTINUE                                                          
C                                                                       
C  DRAW TEMPERATURE LINES                                               
C                                                                       
      T=40.                                                             
      DO 20 I=1,15                                                      
         Y1=FY(TP(I,1))                                                 
         Y2=FY(TP(I,2))                                                 
         X1=FX(T,Y1)                                                    
         X2=FX(T,Y2)                                                    
         CALL LINE(X1,Y1,X2,Y2)                                         
         ITS=NINT(T)                                                    
         IF(ITS.EQ.20) GO TO 19                                         
            X2=X2+0.4                                                   
            Y2=Y2+0.441                                                 
      WRITE (ITIT,101) ITS                                              
C           CALL PWRY(X2,Y2,ITIT,4,12,.83422,0)                         
      CALL WTSTR (X2,Y2,ITIT,12,47,-1)                                  
   19    T=T-10.                                                        
   20 CONTINUE                                                          
C                                                                       
C  TICK MARKS AT 500 MB                                                 
C                                                                       
      Y1=13.2627                                                        
      Y2=13.75                                                          
      T=-52.                                                            
      DO 25 I=1,31                                                      
         T=T+2.                                                         
         IF(AMOD(T,10.).EQ.0.)GO TO 25                                  
         X1=FX(T,Y1)                                                    
         X2=FX(T,Y2)                                                    
         CALL LINE(X1,Y1,X2,Y2)                                         
   25 CONTINUE                                                          
C                                                                       
C  DRAW MIXING RATIO LINES                                              
C                                                                       
      CALL DASHDB (3855)      ! PATTERN = 0000111100001111              
      Y1=FY(1050.)                                                      
      Y2=FY(700.)                                                       
      DO 30 I=1,8                                                       
         X1=FX(TMR(RAT(I),1050.)-ABZ,Y1)                                
         X2=FX(TMR(RAT(I), 700.)-ABZ,Y2)                                
         CALL LINED(X1,Y1,X2,Y2)                                        
C        CALL PWRY(X2,Y2+0.6,LRAT(I),2,10,0,1)                          
      CALL WTSTR (X2,Y2+0.6,LRAT(I),10,0,0)                             
   30 CONTINUE                                                          
C                                                                       
C  DRAW SATURATED ADIABATS                                              
C                                                                       
      CALL DASHDB (31710)     ! PATTERN = 0111101111011110              
      TS=32.                                                            
      DO 40 I=1,7                                                       
         P=1060.                                                        
         TK=TS+ABZ                                                      
         AOS=OS(TK,1000.)                                               
         DO 35 J=1,86                                                   
            P=P-10.                                                     
            ATSA=TSA(AOS,P)-ABZ                                         
            SY(J)=FY(P)                                                 
            SX(J)=FX(ATSA,SY(J))                                        
   35    CONTINUE                                                       
         CALL CURVED(SX,SY,86)                                          
         ITS=NINT(TS)                                                   
      WRITE (ITIT,102) ITS                                              
  102    FORMAT(I2)                                                     
C        CALL PWRY(SX(86),SY(86)+0.6,ITIT,2,10,0,1)                     
      CALL WTSTR (SX(86),SY(86)+0.6,ITIT(1:2),10,0,0)                   
         TS=TS-4.0                                                      
   40 CONTINUE                                                          
C                                                                       
C  DRAW DRY ADIABAT LINES                                               
C                                                                       
      CALL DASHDB (21845)     ! PATTERN = 0101010101010101              
C     CALL DASHD(4444B)                                                 
      T=51.                                                             
      DO 45 I=1,162                                                     
         Y45(I)=66.67*(5.7625544-ALOG(T+ABZ))                           
         T=T-1.0                                                        
   45 CONTINUE                                                          
      T=450.                                                            
      TD=52.                                                            
      DO 55 I=1,20                                                      
         T=T-10.                                                        
         K=0                                                            
         YD=66.67*(ALOG(T)-5.7625544)                                   
         DO 50 J=1,162                                                  
            YPD=Y45(J)+YD                                               
            TX=TD-FLOAT(J)                                              
            IF(YPD.GT.44.061) GO TO 54                                  
            IF(YPD.LT.-.9346217) GO TO 50                               
            XPD=FX(TX,YPD)                                              
            IF(XPD.LT.-19.0)GO TO 54                                    
            IF(XPD.GT.27.1)GO TO 50                                     
            IF(XPD.GT.18.6.AND.T.GT.350.0)GO TO 50                      
               K=K+1                                                    
               SX(K)=XPD                                                
               SY(K)=YPD                                                
   50    CONTINUE                                                       
C                                                                       
   54    CALL CURVED(SX,SY,K)                                           
         ITS=NINT(T)                                                    
      WRITE (ITIT,103) ITS                                              
  103    FORMAT(I3)                                                     
C        IF(ITS .GE. 320) THEN                                          
C           CALL PWRY(SX(K-3),43.0,ITIT,3,10,0,1)                       
C           ELSE                                                        
C           CALL PWRY(-18.0,SY(K-3),ITIT,3,10,0,1)                      
C        END IF                                                         
         X=SX(K-3)                                                      
         Y=SY(K-3)                                                      
         IF(X.LT.-15.0) X = -17.95                                      
         IF(Y.GT.40.0)  Y = 42.9                                        
      CALL WTSTR (X,Y,ITIT(1:3),10,0,0)                                 
   55 CONTINUE                                                          
C                                                                       
      CALL GFLAS2                                                       
      ISKDRW = 1                                                        
      RETURN                                                            
      END                                                               
c
c----------------------------------------------------------------------X
c
      SUBROUTINE WNDBARB (XBASE,YBASE,U,V)                              
                                                                        
C***********************************************************************
C WNDBARB - FOR THE GRAPH PORTION OF GRIN                               
C  THIS ROUTINE DRAWS A SINGLE WIND BARB PER CALL.                      
C                                                                       
C ON INPUT - FOUR VARIABLES COME IN.  XBASE CONTAINS THE HORIZONTAL COOR
C            DINATE OF THE BASE OF THE WIND BARB.  YBASE CONTAINS THE   
C            VERTICAL COORDINATE OF THE BASE OF THE WIND BARB.  U CONTAI
C            THE EAST-WEST WIND COMPONENT IN METERS PER SECOND.  V CONTA
C            THE NORTH-SOUTH WIND COMPONENT IN METERS PER SECOND.       
C                                                                       
C ON OUTPUT - ONE BARB HAS BEEN DRAWN TO UNIT NUMBER 2, WHICH CORRESPOND
C            TO GMETA.CGM, THE GKS OUTPUT META CODE FILE.               
C                                                                       
C ASSUMPTIONS - THIS ROUTINE ASSUMES THAT GKS HAS BEEN OPENED AND A WORK
C            STATION HAS BEEN SET.                                      
C                                                                       
C REVISED BY - JEREMY ASBILL ON MAY 3, 1990.                                       WNDBARB.20    
C***********************************************************************           WNDBARB.21    
                                                                                   WNDBARB.22    
C  INPUT VARIABLE DECLARATIONS ...                                                 WNDBARB.23    
                                                                                   WNDBARB.24    
      REAL      XBASE,YBASE,                                                       WNDBARB.25    
     *          U,V                                                                WNDBARB.26    
                                                                                   WNDBARB.27    
C  LOCAL PARAMETER ...                                                  
C    SC SPECIFIES IN THE NORMALIZED (FRACTIONAL) GRAPHICS COORDINATE    
C       SYSTEM HOW LONG THE BARB SHAFT IS TO BE                         
C    COORDINATE SYSTEMS ARE EXPLAINED IN NCAR GRAPHICS USER'S GUIDE VERS
C    2.00 ON PAGE 46.                                                   
                                                                        
      PARAMETER (SC = 0.05493)                                          
                                                                        
C  LOCAL VARIABLE DECLARATIONS ...                                      
                                                                        
      INTEGER   LLSV                 ! SAVE VARIABLE, SCALING FOR SET   
                                                                        
      LOGICAL DONE                   ! T => SUBROUTINE ENDS, F => LOOP A
                                                                        
      REAL      WINDVCT,             ! WIND VECTOR MAGNITUDE            
     *          FLSV,FRSV,FBSV,FTSV, ! SAVE VARIABLES, FRACTIONAL COORDI
     *          ULSV,URSV,UBSV,UTSV, ! SAVE VARIABLES, INCOMING USER COO
     *          NEWXBASE,            ! FRACTIONAL X COORD. FOR BARB BASE
     *          NEWYBASE,            ! FRACTIONAL Y COORD. FOR BARB BASE
     *          XCOMP,               ! X COMPONENT OF GRAPHICAL VECTOR (
     *          YCOMP,               ! Y COMPONENT OF GRAPHICAL VECTOR (
     *          PK,                  ! PLACE KEEPER                     
     *          FETHLENX,            ! X COMPONENT OF GRAPHICAL VECT. (F
     *          FETHLENY             ! Y COMPONENT OF GRAPHICAL VECT. (F
                                                                        
C  LOCAL ARRAY DECLARATIONS ...                                         
                                                                        
      INTEGER   IJUNK(5)             ! CALCULATION ARRAY FOR SFSGFA     
                                                                        
      REAL      POINTX(3),           ! USED TO SPECIFY POINTS TO DRAW BE
     *          POINTY(3),           ! USED TO SPECIFY POINTS TO DRAW BE
     *          JUNK(7)              ! CALCULATION ARRAY FOR SFSGFA     
                                                                        
C***************************** SUBROUTINE BEGIN ************************
                                                                        
C  INITIALIZE LOOP, BOOLEAN INDICATOR                                   
                                                                        
      DONE = .FALSE.                                                    
                                                                        
C  CALCULATE THE WIND VECTOR MAGNITUDE IN KNOTS                         
                                                                        
      IF ((U .EQ. 0) .AND. (V .EQ. 0)) THEN                             
        WINDVCT = 1.0                                                   
      ELSE                                                              
        WINDVCT = SQRT(U**2 + V**2) * 1.94                              
      END IF                                                            
                                                                        
C  SAVE INCOMING USER COORDINATES AND CHANGE BACK TO NORMALIZED COORDINA
C  DOCUMENTATION FOR SET AND GETSET CAN BE FOUND IN NCAR GRAPHICS USER'S
C  GUIDE VERSION 2.00 ON PAGES 49 (GETSET) AND 53 (SET).                
                                                                        
      CALL GETSET (FLSV,FRSV,FBSV,FTSV,ULSV,URSV,UBSV,UTSV,LLSV)        
      CALL SET    (FLSV,FRSV,FBSV,FTSV, 0.0, 1.0, 0.0, 1.0, 1)          
                                                                        
C  DETERMINE WHERE THE BASE OF THE BARB IS IN THE NORMALIZED COORDINATES
                                                                        
      NEWXBASE = (XBASE - ULSV)/(URSV - ULSV)                           
      NEWYBASE = (YBASE - UBSV)/(UTSV - UBSV)                           
                                                                        
C  CALCULATE THE X DISTANCE AND Y DISTANCE FROM THE BASE OF THE BARB THA
C  DEFINES THE BARBS TIP (NORMALIZED COORD'S)                           
                                                                        
      XCOMP = -SC * U * 1.94/WINDVCT                                    
      YCOMP = -SC * V * 1.94/WINDVCT                                    
                                                                        
C  DETERMINE THE ACTUAL LOCATION IN NORMALIZED COORDINATES OF THE BARB'S
                                                                        
      POINTX(1) = NEWXBASE + XCOMP                                      
      POINTY(1) = NEWYBASE + YCOMP                                      
                                                                        
C  DRAW THE BARB SHAFT, DOCUMENTATION FOR THE LINE SUBROUTINE CAN BE FOU
C  IN NCAR GRAPHICS USER'S GUIDE VERSION 2.00 ON PAGE 50                
                                                                        
      CALL LINE (NEWXBASE,NEWYBASE,POINTX(1),POINTY(1))                 
                                                                        
C  DETERMINE THE FEATHER LENGTH                                         
                                                                        
      FETHLENX = 0.3 * YCOMP                                            
      FETHLENY = -0.3 * XCOMP                                           
                                                                        
C  SET THE PLACE KEEPER AND BOOST THE WIND MAGNITUDE                    
                                                                        
      PK = 0.9                                                          
      WINDVCT = WINDVCT + 2.5                                           
                                                                        
C  BEGIN MAKING FEATHERS                                                
                                                                        
10    CONTINUE                                                          
                                                                        
C    DRAW A FLAG FOR EVERY 50 KNOTS WIND MAGNITUDE                      
                                                                        
      IF (WINDVCT .GE. 50.0) THEN                                       
                                                                        
C      DETERMINE THE POSITION OF THE FLAG TIP, POINT_(2)                
C      AND DETERMINE POSITION WHERE FLAG BOTTOM MEETS THE SHAFT, POINT_(
                                                                        
        POINTX(2) = POINTX(1) + FETHLENX + 0.0005                       
        POINTY(2) = POINTY(1) + FETHLENY + 0.0005                       
        POINTX(3) = PK * XCOMP + NEWXBASE                               
        POINTY(3) = PK * YCOMP + NEWYBASE                               
                                                                        
C      DRAW FLAG                                                        
                                                                        
        CALL LINE (POINTX(1),POINTY(1),POINTX(2),POINTY(2))             
        CALL LINE (POINTX(3),POINTY(3),POINTX(2),POINTY(2))             
                                                                        
C      FILL IN FLAG, DOCUMENTATION FOR SFSGFA CAN BE FOUND IN NCAR      
C      GRAPHICS GUIDE TO NEW UTILITIES VERSION 3.00 ON PAGE 4-8         
                                                                        
        CALL SFSETR ('SP',0.000001)                                     
        CALL SFSGFA (POINTX,POINTY,3,JUNK,5,IJUNK,7,2)                  
                                                                        
C      REMOVE 50 KNOTS FROM WIND MAGNITUDE (ALREADY DRAWN IN)           
                                                                        
        WINDVCT = WINDVCT - 50.0                                        
                                                                        
C      DETERMINE NEW BEGIN POINT FOR NEXT FLAG OR FEATHER               
                                                                        
        PK = PK - 0.05                                                  
        POINTX(1) = PK * XCOMP + NEWXBASE                               
        POINTY(1) = PK * YCOMP + NEWYBASE                               
        PK = PK - 0.1                                                   
                                                                        
C    DRAW A FULL FEATHER FOR WIND MAGNITUDE OF EVERY 10 KNOTS           
                                                                        
      ELSE IF (WINDVCT .GE. 10.0) THEN                                  
                                                                        
C      CALCULATE POSITION OF FEATHER END                                
                                                                        
        POINTX(2) = POINTX(1) + FETHLENX + 0.0005                       
        POINTY(2) = POINTY(1) + FETHLENY + 0.0005                       
                                                                        
C      DRAW FEATHER                                                     
                                                                        
        CALL LINE (POINTX(1),POINTY(1),POINTX(2),POINTY(2))             
                                                                        
C      REMOVE 10 KNOTS FROM WIND MAGNITUDE (ALREADY DRAWN IN)           
                                                                        
        WINDVCT = WINDVCT - 10.0                                        
                                                                        
C      DETERMINE NEW START POINT FOR NEXT FEATHER OR FLAG               
                                                                        
        POINTX(1) = PK * XCOMP + NEWXBASE                               
        POINTY(1) = PK * YCOMP + NEWYBASE                               
        PK = PK - 0.1                                                   
                                                                        
C    DRAW A HALF FEATHER FOR EVERY 5 KNOTS OF WIND MAGNITUDE            
                                                                        
      ELSE IF (WINDVCT .GE. 5.0) THEN                                   
                                                                        
C      CALCULATE POSITION OF TIP OF HALF FEATHER                        
                                                                        
        POINTX(2) = POINTX(1) + 0.5 * FETHLENX + 0.0005                 
        POINTY(2) = POINTY(1) + 0.5 * FETHLENY + 0.0005                 
                                                                        
C      DRAW IN FEATHER                                                  
                                                                        
        CALL LINE (POINTX(1),POINTY(1),POINTX(2),POINTY(2))             
                                                                        
C      TELL LOOP TO QUIT                                                
                                                                        
        DONE = .TRUE.                                                   
      ELSE                                                              
        DONE = .TRUE.                                                   
      END IF                                                            
                                                                        
C  IF THERE IS STILL MORE WIND MAGNITUDE (>= 5 KNOTS) LOOP AGAIN        
                                                                        
      IF (.NOT. DONE) GOTO 10                                           
                                                                        
C  RESET USER COORDINATES TO THE INCOMING VALUES                        
                                                                        
      CALL SET (FLSV,FRSV,FBSV,FTSV,ULSV,URSV,UBSV,UTSV,LLSV)           
                                                                        
C****************************** SUBROUTINE END *************************
                                                                        
      RETURN                                                            
      END                                                               

