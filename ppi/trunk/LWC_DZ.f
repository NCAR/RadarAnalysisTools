
c----------------------------------------------------------------------X
c
      SUBROUTINE LWC_DZ(DAT,IOUT,IIN1,C1,C2,C3,C4,BDVAL,MNGATE,MXGATE,
     X                  NANG,AZA,ELA,ITPOLD,H0,R0,DROLD,MXR,MXA,MXF)
C
C  FUNCTION - LWC_DZ: F(OUT)=LWC(DBZ,adiabatic LWC), LWC in g/m^3
C
C     From DBZ = (2-B)*10*log(LWC_a)-10*log(No)+10*log(m6/m3^2)
C               +10*log(Den/Do)+20*log(PI*Rhow/6)+B*10*log(LWC),
C               where B=C1 (and C2), No=C3 
C               and Density = Do*exp(-kz), 10*log(e)=4.342945
C     this routine returns different fields according to C4
C
C              Calculate (0,1,2) from input dBZ field
C        (0) - Fraction of adiabatic liquid water content
C        (1) - Liquid water content (g/m^3)
C        (2) - ALog10[Liquid water content (g/m^3)]
C              Calculate (3) from adiabatic LWC (See ADIABLWC).
C        (3) - Reflectivity (dBZ) equivalent of adiabatic lwc, i.e.
C              assume lwc is adiabatic (LWC=LWCad) and compute dBZ.
C        (4) - Adiabatic lwc.
C        (5) - Adiabatic diameter (micron)
C        (6) - Adiabatic concentration (n/cm3)
C
C  SETLWC COMMAND: Previously executed to calculate adiabatic profiles
C                  from input cloud base conditions of (p,t,z)
C     Note:  Uses the first (NC=1) adiabatic profile computed.
C
C     IOUT   - OUTPUT FIELD NUMBER
C     IIN1   -  INPUT   "      "
C
C     AZA,ELA     - AZIMUTH AND ELEVATION ANGLES OF RADAR BEAMS
C     ITPOLD      - SCANNING MODE [RHI (3): AZA CONTAINS ELEVATION ANGLE;
C                                           ELA     "     AZIMUTH    "  ]
C     R0,DROLD    - INITIAL RANGE (KM) AND GATE SPACING
C     H0          - Z COORDINATE OF RADAR (KM)
C
      PARAMETER (NAMX=500,NCMX=10)
      COMMON/LWC/PBASE(NCMX),TBASE(NCMX),ZBASE(NCMX),B1(NCMX),
     X     B2(NCMX),B3(NCMX),HMAX(NCMX),NAL(NCMX),PALT(NAMX,NCMX),
     X     ADBLWC(NAMX,NCMX),ADBDBZ(NAMX,NCMX),ADBDIA(NAMX,NCMX),
     X     ADBCON(NAMX,NCMX),NC

      DIMENSION DAT(MXR,MXA,MXF),AZA(MXA,2),ELA(MXA,2)

      DATA RE,REI/17000.0,1.17647E-04/
      DATA TORAD/0.017453293/
      DATA R1,R2,R3/0.85,0.85,212.0/
      DATA ADLWCMN,RKZ,DENW/0.01,0.1,5.62/

C     SET COEFFICIENTS FOR LWC = LWC(DBZ,adiabatic LWC)
C
      IF(C1.EQ.0.0.AND.C2.EQ.0.0.AND.C3.EQ.0.0)THEN
         A1=R1
         A2=R2
         A3=R3
      ELSE
         A1=C1
         A2=C2
         A3=C3
      END IF
      IOPT=C4
      IF(A2.NE.0)THEN
         A2I=1.0/A2
      ELSE
         A2I=0.0
      END IF
      ALOG_N=10.0*ALOG10(A3)

C     INTERPOLATE FOR ADLWC AT CURRENT RANGE GATE HEIGHT (HT).
C
      DO 100 J=1,NANG
         IF(ITPOLD.EQ.3)THEN
            SINE=SIN(AZA(J,1)*TORAD)
            COSE=COS(AZA(J,1)*TORAD)
         ELSE
            SINE=SIN(ELA(J,1)*TORAD)
            COSE=COS(ELA(J,1)*TORAD)
         END IF
         DO 90 I=MNGATE,MXGATE
            DAT(I,J,IOUT)=BDVAL
            DBZ=DAT(I,J,IIN1)
            IF(DBZ.EQ.BDVAL)GO TO 90
            RNG=R0+(I-1)*DROLD
            HRNG=RNG*COSE
            HT=H0+RNG*SINE+0.5*HRNG*HRNG*REI
            IF(HT.LT.ZBASE(1).OR.HT.GT.HMAX(1).OR.NAL(1).LE.1)GO TO 90
            DO 50 N=2,NAL(1)
               IF(HT.GT.PALT(N,1))GO TO 50
               DENOM=PALT(N,1)-PALT(N-1,1)
               SLOPELWC=(ADBLWC(N,1)-ADBLWC(N-1,1))/DENOM
               SLOPEDBZ=(ADBDBZ(N,1)-ADBDBZ(N-1,1))/DENOM
               SLOPEDIA=(ADBDIA(N,1)-ADBDIA(N-1,1))/DENOM
               ADLWC=ADBLWC(N-1,1)+(HT-PALT(N-1,1))*SLOPELWC
               ADDBZ=ADBDBZ(N-1,1)+(HT-PALT(N-1,1))*SLOPEDBZ
               ADDIA=ADBDIA(N-1,1)+(HT-PALT(N-1,1))*SLOPEDIA
               RDLWC=10.0*ALOG10(ADLWC)
               DENA=4.3429*RKZ*(HT-ZBASE(1))
               GO TO 60
 50         CONTINUE
            ADLWC=0.0
 60         CONTINUE
            IF(ADLWC.LE.ADLWCMN)THEN
               DAT(I,J,IOUT)=BDVAL

            ELSE
               ALWC=A2I*(DBZ-((2.0-A1)*RDLWC-ALOG_N+DENA+DENW))
               ZLWC=10.0**(0.1*ALWC)

               IF(IOPT .EQ. 0)THEN
                  DAT(I,J,IOUT)=ZLWC/ADLWC

               ELSE IF(IOPT .EQ. 1)THEN
                  DAT(I,J,IOUT)=ZLWC

               ELSE IF(IOPT .EQ. 2)THEN
                  DAT(I,J,IOUT)=ALWC

               ELSE IF(IOPT .EQ. 3)THEN
                  DAT(I,J,IOUT)=ADDBZ

               ELSE IF(IOPT .EQ. 4)THEN
                  DAT(I,J,IOUT)=ADLWC

               ELSE IF(IOPT .EQ. 5)THEN
                  DAT(I,J,IOUT)=ADDIA

               END IF
            END IF
 90      CONTINUE
 100  CONTINUE
      RETURN
      END


