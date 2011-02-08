c
c----------------------------------------------------------------------X
c
      SUBROUTINE ELTOPO(DAT,IOUT,IIN1,NIN2,C1,C2,C3,NSCTP,FXOLD,BDVAL,
     X     MNGATE,MXGATE,MANG,RNG,AZA,ELA,AZROT,ITPOLD,MXR,MXA,MXF,H0,
     X     ISW)
C
C  FUNCTION - ELTOPO: F(OUT)=Elevation angle of topography field.
C                            Elevation angle of horizon.
C                            Locations where the beam is blocked.
C
C     Call a TOPO function first to get topography height (m) field.
C
C     NIN2   - (ELEVANG) - Radar elevation angle of the topographic height
C              (HORIZON) -   "       "       "    "  "  horizon
C              (BLOCKED) - Fractional blockage at the radar sample point
C
C     C1     - Nominal radar beamwidth (deg)
C     C2     - First range (km) for calculations
C     C3     - If printing out values, do so for fixed angle .le. C3
C     NSCTP  - Contains printout control in the form PRNTrraa (PRNT0210)
C              to print every rr gate and aa beam
C     IOUT   - Output field number
C     IIN1   -  Input   "      "
C     RNG       - Distance (km) to the range gates
C     AZA       - Angle which varies during the scan (azimuth or elevation)
C     FXOLD     - Fixed angle during scan.
C     ITPOLD    - Scanning mode [RHI (3): AZA contains elevation angle;
C                              otherwise, AZA contains azimuth angle]
C     X0,Y0,HO  - (X,Y,Z) coordinates for this radar
C     AZROT     - Array of angle corrections (usually all 0s)
C
      DIMENSION DAT(MXR,MXA,MXF)
      DIMENSION RNG(MXR,2),AZA(MXA,2),ELA(MXA,2)
      DIMENSION AZROT(8)
      CHARACTER*8 NIN2,TYPE,NSCTP
      LOGICAL FRSTGATE,PRINT

      DATA RE,REI/17000.0,1.17647E-04/
      DATA TORAD,TODEG/0.017453293,57.29577951/

      TYPE=NIN2
      BWIDTH=C1
      FRST_RNG=C2
      IF(FRST_RNG.LE.0.0)FRST_RNG=1.0
      IF(BWIDTH.LE.0.0)BWIDTH=1.0
      BWHALF=0.5*BWIDTH
      FXMAX=C3

      PRINT=.FALSE.
      IF(NSCTP(1:4).EQ.'PRNT'.AND.FXOLD.LE.FXMAX)THEN
         PRINT=.TRUE.
         READ(NSCTP(5:8),11)IMOD,JMOD
 11      FORMAT(2I2)
         IF(IMOD.EQ.0)IMOD=9999
         IF(JMOD.EQ.0)JMOD=9999
      END IF
      
      IF(PRINT)THEN
         PRINT *,'ELTOPO - ',TYPE,BWIDTH,INT(FRST_RNG),NSCTP
      END IF

C     Note: If ISW=1, coordinates only at original sampling locations
C              ISW=2, coordinates at regular range-angle grid
C     RHI (3) - AZA contains elevation angle and FXOLD = azimuth
C     Others  - AZA contains azimuth   angle and FXOLD = elevation
C
      DO 100 J=1,MANG

C        RHI scan:
C
         IF(ITPOLD.EQ.3)THEN
            IF(ISW.EQ.2)THEN
               AZRAD=FXOLD-AZROT(ITPOLD)
            ELSE
               AZRAD=ELA(J,ISW)
            END IF
            ELRAD=AZA(J,ISW)

C        All other scans:
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

         FRSTGATE=.FALSE.
         DO 90 I=MNGATE,MXGATE

            DAT(I,J,IOUT)=BDVAL
            IF(DAT(I,J,IIN1).EQ.BDVAL)GO TO 90

            IF(RNG(I,ISW).GT.FRST_RNG)THEN
               ZDAT=0.001*DAT(I,J,IIN1)
               DELZ=ZDAT-H0
               HRNG=RNG(I,ISW)*COSE
c               ARC=(DELZ-0.5*HRNG*HRNG*REI)/RNG(I,ISW)
               ARC=DELZ/RNG(I,ISW)
               IF(ABS(ARC).LE.1.0)THEN
                  ELEV=TODEG*ASIN(ARC)
               ELSE
                  ELEV=BDVAL
               END IF
               IF(.NOT.(FRSTGATE))THEN
                  IF(ELEV.NE.BDVAL)THEN
                     ELEVMX=ELEV
                     FRSTGATE=.TRUE.
                  END IF
               ELSE
                  IF(ELEV.GT.ELEVMX)ELEVMX=ELEV
               END IF

C              The beam is nominally defined by beamwidth and DELANG 
C              is the angular distance between center of beam and the
C              maximum elevation angle needed to clear terrain along
C              the radial.  DELANG > 0, beam points above terrain; 
C                                  < 0, beam points below terrain.
C
               DELANG=ELRAD-ELEVMX
               FBLOCKED=(BWHALF-DELANG)/BWIDTH

               IF(FBLOCKED.LT.0.0)THEN
                  FBLOCKED=0.0
               ELSE IF(FBLOCKED.GE.1.0)THEN
                 FBLOCKED=1.001
               END IF
               
               IF(TYPE.EQ.'ELEVANG')THEN
                  DAT(I,J,IOUT)=ELEV
               ELSE IF(TYPE.EQ.'HORIZON')THEN
                  DAT(I,J,IOUT)=ELEVMX
               ELSE IF(TYPE.EQ.'BLOCKED')THEN
                  DAT(I,J,IOUT)=FBLOCKED
               END IF
            
               IF(PRINT)THEN
                  IF(MOD(J,JMOD).EQ.0 .AND. MOD(I,IMOD).EQ.0)THEN
                     WRITE(6,71)I,RNG(I,ISW),AZRAD,ELRAD,ZDAT,
     +                    DELZ,ELEV,ELEVMX,DELANG,FBLOCKED
 71                  FORMAT(' ELTOPO: rae=',I4,3F8.3,' z,dz(m)=',
     +                    2F8.3,' el(z)-elmx-del-fb=',4F8.3)
                  END IF
               END IF

            END IF

 90      CONTINUE
 100  CONTINUE
      
      RETURN
      END
