
      PROGRAM TST_LWC

      PARAMETER (NAMX=500,NCMX=10)
      COMMON/LWC/PBASE(NCMX),TBASE(NCMX),ZBASE(NCMX),B1(NCMX),
     X     B2(NCMX),B3(NCMX),HMAX(NCMX),NAL(NCMX),PALT(NAMX,NCMX),
     X     ADBLWC(NAMX,NCMX),ADBDBZ(NAMX,NCMX),ADBDIA(NAMX,NCMX),
     X     ADBCON(NAMX,NCMX),NC

      CHARACTER*8 INDAT(10)

      OPEN(UNIT=5,FILE='setlwc.inp',STATUS='OLD')
      READ(5, 7)(INDAT(I),I=1,10)
      READ(5, 7)(INDAT(I),I=1,10)
 7    FORMAT(10A8)
      WRITE(6, 7)(INDAT(I),I=1,10)
      CALL SETLWC(INDAT)
      STOP
      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE SETLWC(INDAT)
C
C     Compute vertical profile of adiabatic liquid water content from 
C     input cloud base conditions of (p,t,z)=(PB,TB,ZB).  Routine ADIABLWC
C     returns height (PALT) and adiabatic liquid water content (ADBLWC) 
C     arrays at 2-mb increments from cloud base pressure (P).  
C     LWC_DZ:  Interpolates for ADLWC at current range gate height (HT).
C     Default (p,t,z) from Florida CaPE91
C     NC - Counter for the current adiabatic profile
C
      CHARACTER*8 INDAT(10)

      PARAMETER (NAMX=500,NCMX=10)
      COMMON/LWC/PBASE(NCMX),TBASE(NCMX),ZBASE(NCMX),B1(NCMX),
     X     B2(NCMX),B3(NCMX),HMAX(NCMX),NAL(NCMX),PALT(NAMX,NCMX),
     X     ADBLWC(NAMX,NCMX),ADBDBZ(NAMX,NCMX),ADBDIA(NAMX,NCMX),
     X     ADBCON(NAMX,NCMX),NC
      DIMENSION ENVPRS(NAMX),ADBTMP(NAMX),DEWTK(NAMX)

      DATA R1,R2,R3/943.0,22.4,0.6/
      DATA S1,S2,S3/0.85,0.85,212.0/

      NC=NC+1
      WRITE(6,11)(INDAT(I),I=2,10)
 11   FORMAT(1X,'SETLWC: ',9A8)
      READ(INDAT,13)PB,TB,ZB,B1(NC),B2(NC),B3(NC)
 13   FORMAT(/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0)
      IF(PB.EQ.0.0.AND.TB.EQ.0.0.AND.ZB.EQ.0.0)THEN
         PBASE(NC)=R1
         TBASE(NC)=R2
         ZBASE(NC)=R3
      ELSE
         PBASE(NC)=PB
         TBASE(NC)=TB
         ZBASE(NC)=ZB
      END IF
      IF(B1(NC).EQ.0.0.AND.B2(NC).EQ.0.0.AND.B3(NC).EQ.0.0)THEN
         B1(NC)=S1
         B2(NC)=S2
         B3(NC)=S3
      END IF

      NAL(NC)=0
      CALL ADIABLWC(PBASE,TBASE,ZBASE,B3,PALT,ADBLWC,ADBDBZ,ADBDIA,
     X     ADBCON,HMAX,NAMX,NCMX,NC,NAL,ENVPRS,ADBTMP,DEWTK)

      RETURN
      END
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
c      NAL(NC)=1.001+(PBASE(NC)-500.0)/PINC
      NAL(NC)=1.001+(PBASE(NC)-100.0)/PINC
      IF(NAL(NC).GT.NAMX)NAL(NC)=NAMX
      ALOG_N=10.0*ALOG10(B3(NC))
      BASEC=B3(NC)

C     Compute equivalent potential temperature (saturated adiabat)
C     and potential temperature (dry adiabat) through cloud base 
C     temperature (BASET) and pressure (BASEP).
C     Compute mixing ratio (MIXBASE g/kg) at cloud base
C
      BASEP=PBASE(NC)
      BASET=TBASE(NC)+ABSC
      BASEZ=ZBASE(NC)
      AOS=OS(BASET,BASEP)
      AOD=OD(BASET,BASEP)
      BASEQ=W(BASET,BASEP)

      WRITE(6,51)BASEP,BASET,AOS,BASEQ,AOD
 51   FORMAT(1X,'CLOUD BASE CONDITIONS: P=',F6.1,' T=',F6.1,
     +     ' ThetaE=',F6.1, ' Mixing ratio=',F6.1,' Theta=',F6.1)

      WRITE(6,11)PBASE(NC),TBASE(NC),AOS,BASEQ,ZBASE(NC)
 11   FORMAT(/,1X,'ADIABLWC: CLOUD BASE P,T,EPOT,Q,Z=',4F8.1,F8.3,/)

      WRITE(6,13)
 13   FORMAT(2X,'NC   N    Palt    Pres T_Cloud  MixRat',
     X     '    Conc     LWC     DBZ     DIA')
      DO 200 N=1,NAL(NC)
         ENVPRS(N)=BASEP-(N-1)*PINC
         ADBTMP(N)=TSA(AOS,ENVPRS(N))
         ADBMIX=W(ADBTMP(N),ENVPRS(N))
         IF(N.EQ.1)THEN
            CBMIX=ADBMIX
         END IF
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
         SAT=OS(ADBTMP(N),ENVPRS(N))
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
         RLWC=(CBMIX-ADBMIX)*0.34838*ENVPRS(N)/(ADBTC+273.16)
c         IF(MOD(N,5).EQ.0)THEN
            WRITE(6,101)NC,N,PALT(N,NC),ENVPRS(N),ADBTC,ADBMIX,
     +           ADBCON(N,NC),ADBLWC(N,NC),ADBDBZ(N,NC),ADBDIA(N,NC),
     +        SAT,RLWC
 101        FORMAT(2I4,F8.3,7F8.2,2F8.2)
c         END IF
 200  CONTINUE

      RETURN
      END
c
c----------------------------------------------------------------------X
c
      FUNCTION ESAT(T)
C     
C     SATURATION VAPOR PRESSURE FOR INPUT (T)
C     ESAT(MILLIBARS),T(KELVIN)
C     
      DATA ABZ/273.16/
C     
      TC=T-ABZ
      ESAT=6.1078*EXP((17.2693882*TC)/(TC+237.3))

      RETURN
      END
c
c----------------------------------------------------------------------X
c
      FUNCTION OD(T, P)                                                
C     
C     OD(T,P) DRY ADIABATIC LINE THROUGH T,P (KELVIN)
C     OUTPUT POTENTIAL TEMPERATURE FROM INPUT (T,P)
C     OD AND T(KELVIN), P(MILLIBARS)
C     
      OD = T*(1000./P)**.286
      RETURN                                                            
      END                                                               
c
c----------------------------------------------------------------------X
c
      FUNCTION OS(T,P)
C     
C     OS(T,P) SATURATED ADIABAT LINE THROUGH T,P (KELVIN)
C     OUTPUT EQUIVALENT POTENTIAL TEMPERATURE FROM INPUT (T,P)
C     OS AND T(KELVIN), P(MILLIBARS)
C     
      OS=T*((1000./P)**.286)/(EXP(-2.6518986*W(T,P)/T))
      RETURN
      END
c
c----------------------------------------------------------------------X
c
      FUNCTION TDRY(OD, P)                                                
C     
C     AIR TEMPERATURE ALONG A DRY ADIABAT
C     OUTPUT AIR TEMPERATURE TDRY (KELVIN)
C     FROM INPUT (OD,P) in Kelvin, millibars
C     
      A = OD
      TDRY = A*(P/1000.)**.286
      RETURN                                                            
      END                                                               
c
c----------------------------------------------------------------------X
c
      FUNCTION TMR(W,P)
C
C     TMR(AW,P) TEMPERATURE ON MIXING RATIO AW AT LEVEL P (KELVIN)              
C     TMR(KELVIN),W(GRAMS WATER VAPOR/KILOGRAM DRY AIR),P(MILLIBAR)             
C     ALOG10 IS LOG TO THE BASE TEN.                                        
C
      IF(W.LE.0.0)W=0.01
      X =  ALOG10(   W*P/(622.+ W)  )
      TMR=10.**(.0498646455*X+2.4082965)-7.07475+38.9114*((10.**(
     1     .0915*X ) - 1.2035 )**2 )
      RETURN
      END
c
c----------------------------------------------------------------------X
c
      FUNCTION TSA(OS,P)
C     
C     TSA(OS,P) TEMPERATURE ON SATURATED ADIABAT AOS AT LEVEL P (KELVIN)
C     SIGN(A,B) REPLACES THE ALGEBRAIC SIGN OF A WITH THE SIGN OF B
C     TSA AND OS(KELVIN), P(MILLIBARS)
C     
      A=OS
      TQ=253.16
      D=120
C     
      DO 1 I=1,50
         D=D/2
C     
C     IF THE TEMPERATURE DIFFERENCE,X, IS SMALL, EXIT THIS LOOP
C     
         X=A*EXP(-2.6518986*W(TQ,P)/TQ)-TQ*((1000./P)**.286)
         IF (ABS(X).LT.0.001) GO TO 2
         TQ=TQ+SIGN(D,X)
 1    CONTINUE
 2    TSA=TQ

      RETURN
      END
c
c----------------------------------------------------------------------X
c
      FUNCTION W(T,P)
C     
C     W(T,P) MIXING RATIO LINE THROUGH T,P (GM/KG)
C     W(GRAMS WATER VAPOR/KILOGRAM DRY AIR), P(MILLIBARS)
C     
      IF (T.GE.999.) GO TO 10
      X=ESAT(T)
      W=622.*X/(P-X)

      RETURN

 10   W=10.0

      RETURN
      END
c
c----------------------------------------------------------------------X
c
      FUNCTION Z(PT,P,T,TD,N)
C
C     Z(PTOP,P,T,TD,N) THICKNESS IN METERS FROM P(1) TO PTOP                    
C     P IS PRESSURE IN MILLIBARS                                                
C     T IS TEMPERATURE IN KELVIN                                                
C     TD IS DEWPOINT IN KELVIN                                                  
C
      DIMENSION T(1),P(1),TD(1)

      Z = 0.0
      IF(PT.LT.P(N))GO TO 20
      I =  0

 9    I = I+1
      J  = I+1
      IF(PT.GE.P(J)) GO TO 10
      A1=T(J)*(1.+.0006078*W(TD(J),P(J)))
      A2=T(I)*(1.+.0006078*W(TD(I),P(I)))
      Z = Z+14.64285*(A1+A2)*(ALOG(P(I)/P(J)))
      GO TO 9

 10   CONTINUE
      A1=T(J)*(1.+.0006078*W(TD(J),P(J)))
      A2=T(I)*(1.+.0006078*W(TD(I),P(I)))
      Z = Z+14.64285*(A1+A2)*(ALOG(P(I)/PT ))
      RETURN

 20   Z=-1.0

      RETURN
      END



