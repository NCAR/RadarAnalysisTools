c
c----------------------------------------------------------------------X
c
      SUBROUTINE ADIABLWC(PBASE,TBASE,ZBASE,B3,PALT,ADBLWC,ADBDBZ,
     X     ADBDIA,ADBCON,HMAX,NAMX,NCMX,NC,NAL,ENVPRS,ADBTMP,DEWTK)
C
C     GIVEN CLOUD BASE CONDITIONS (P,T,Z), COMPUTE ADIABATIC LWC
C
C     From the cloud base (p,T), first calculate temperature of the 
C     saturated adiabat, then decrement pressure while calculating
C     the adiabatic values of temperature, mixing ratio and liquid
C     water content.  Use the adiabatic temperature to get a dew point
C     temperature and for calculating a thickness (pressure altitude).
C
C           dBZ = 20*log(Q_a) - 10*log(N_o) + 4.3429*k*z + 5.62
C           rho = rho_o*exp(-k*z); where k=0.1
C
C     INPUT:
C           PBASE - CLOUD BASE PRESSURE (MB)
C           TBASE -   "     "  TEMPERATURE (DEG C)
C           ZBASE -   "     "  HEIGHT (KM)
C     OUTPUT:
C            PALT   - ALTITUDES (KM)
C            ADBLWC - ADIABATIC LIQUID WATER CONTENT (G/M3)
C            ADBDBZ -     "     RADAR REFLECTIVITY FACTOR
C            ADBDIA -     "     PARTICLE DIAMETER (MICRON)
C            ADBCON -     "     CONCENTRATION (N/CM3)
C            HMAX   - MAXIMUM ALTITUDE (KM)
C
C     PINC - INCREMENT IN PRESSURE (MB)
C
      DIMENSION ADBLWC(NAMX,NCMX),ADBDBZ(NAMX,NCMX),ADBDIA(NAMX,NCMX)
      DIMENSION ADBCON(NAMX,NCMX)
      DIMENSION PALT(NAMX,NCMX),ENVPRS(NAMX),ADBTMP(NAMX),DEWTK(NAMX)
      DIMENSION PBASE(NCMX),TBASE(NCMX),ZBASE(NCMX),HMAX(NCMX),NAL(NCMX)
      DIMENSION B3(NCMX)
      DATA DENC,ABSC,RK,RKZ/0.34838,273.16,0.286,0.1/
      DATA DENW/5.62/

      PINC=2.0
      NAL(NC)=1.001+(PBASE(NC)-500.0)/PINC
      IF(NAL(NC).GT.NAMX)NAL(NC)=NAMX
      ALOG_N=10.0*ALOG10(B3(NC))
      BASEC=B3(NC)

      BASEP=PBASE(NC)
      BASET=TBASE(NC)+ABSC
      BASEZ=ZBASE(NC)
      AOS=OS(BASET,BASEP)
      BASEQ=W(BASET,BASEP)

      WRITE(6,11)PBASE(NC),TBASE(NC),AOS,BASEQ,ZBASE(NC)
 11   FORMAT(/,1X,'ADIABLWC: CLOUD BASE P,T,EPOT,Q,Z=',4F8.1,F8.3,/)

      DO 200 N=1,NAL(NC)
         ENVPRS(N)=BASEP-N*PINC
         ADBTMP(N)=TSA(AOS,ENVPRS(N))
         ADBMIX=W(ADBTMP(N),ENVPRS(N))
         IF(ADBMIX.LT.0.0)ADBMIX=0.0
         DEWTK(N)=TMR(ADBMIX,ENVPRS(N))-5.0
         ADBLWC(N,NC)=(DENC*ENVPRS(N)*(BASEQ-ADBMIX))/ADBTMP(N)
         IF(ADBLWC(N,NC).LT.0.0)ADBLWC(N,NC)=0.0
         IF(N.LE.1)THEN
            PALT(N,NC)=BASEZ
         ELSE
            PALT(N,NC)=Z(ENVPRS(N),ENVPRS,ADBTMP,DEWTK,N)
            PALT(N,NC)=0.001*PALT(N,NC)+BASEZ
         END IF
         HMAX(NC)=PALT(N,NC)
         ADBTC=ADBTMP(N)-ABSC
         IF(ADBLWC(N,NC).GT.0.0)THEN
            RDLWC=10.0*ALOG10(ADBLWC(N,NC))
            HT=PALT(N,NC)-BASEZ
            DENA=4.3429*RKZ*HT
            ADBDBZ(N,NC)=2.0*RDLWC-ALOG_N+DENA+DENW
         ELSE
            ADBDBZ(N,NC)=-80.0
         END IF
         ALOG_D=(ADBDBZ(N,NC)-ALOG_N+DENA-60.0)/60.0
         ADBDIA(N,NC)=1000.0*(10.0**ALOG_D)
         ADBCON(N,NC)=BASEC*EXP(-RKZ*HT)
c         IF(MOD(N,5).EQ.0)THEN
            WRITE(6,101)NC,N,PALT(N,NC),ENVPRS(N),ADBTC,ADBMIX,
     +           ADBCON(N,NC),ADBLWC(N,NC),ADBDBZ(N,NC),ADBDIA(N,NC)
 101        FORMAT('NC,N,Z,P,T,M,CON,LWC,DBZ,DIA=',2I4,F8.3,7F8.2)
c         END IF
 200  CONTINUE

      RETURN
      END


