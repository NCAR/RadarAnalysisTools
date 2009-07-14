c
c----------------------------------------------------------------------X
c
      SUBROUTINE LISTFLD(NAMFLD,NFLDS,PRMN,PRMX,PAMN,PAMX,RSKIP,ASKIP,
     X                   LSTNAM,NAMLST,NLST,PZMN,PZMX,H0)
C
C  LIST SEVERAL FIELDS AS A FUNCTION OF RANGE WITHIN THE CURRENT SWEEP
C
C     NLST      - NUMBER OF FIELDS TO PRINT OUT
C     LSTNAM    - NAMES   "    "    "   "    "
C     IRTYPE    - TYPE OF PLOT (RNGE OR ANGL)
C     PRMN,PRMX - MINIMUM AND MAXIMUM RANGES (KM) TO BE LISTED
C     PAMN,PAMX -    "     "     "    ANGLES (DG)  "  "    "
C     PZMN,PZMX -    "     "     "    HEIGHT (KM)  "  "    "
C     R,ASKIP   - RANGE,ANGLE SKIPPING FACTOR WITHIN THE SWEEP
C       NANG(1) - NUMBER OF ANGLES IN THE CURRENT SWEEP
C     Z (MSL)   - RADAR HEIGHT + Z ABOVE RADAR + CURVATURE CORRECTION
C                 Z ABOVE RADAR = SLANT RANGE * SIN(ELEV)
C                 CURV CORRECT  = (HOR. RANGE)^2 / (4/3 EARTH DIAMETER)
C
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'

      CHARACTER*8 NAMFLD(MXF),LSTNAM(MXF),NAMLST(MXF)
      DIMENSION FLDAT(MXF)
      DATA RE,TORAD/17000.0,0.017453293/

      IRB=1.001+(PRMN-R0)/DROLD
      IRE=1.001+(PRMX-R0)/DROLD
      IF(IRB.LT.1)IRB=1
      IF(IRE.GE.MXR)IRE=MXR-1

      ISKIP=RSKIP
      JSKIP=ASKIP
      IF(ISKIP.LE.0)ISKIP=1
      IF(JSKIP.LE.0)JSKIP=1

C     LOOP OVER ALL ANGLES IN THE CURRENT SWEEP
C
      DO 30 J=1,NANG(1),JSKIP
         PANG=AZA(J,1)
         IF(PANG.LT.0.0)THEN
            TANG=PANG+360.0
         ELSE
            TANG=PANG
         END IF
         IF(TANG.LT.PAMN.OR.TANG.GT.PAMX)GO TO 30
         IF(ITPOLD.EQ.3)THEN
            SINE=SIN(AZA(J,1)*TORAD)
            COSE=SIN(AZA(J,1)*TORAD)
         ELSE
            SINE=SIN(ELA(J,1)*TORAD)
            COSE=SIN(ELA(J,1)*TORAD)
         END IF
         IF(ITPOLD.EQ.3)THEN
            WRITE(6,13)TANG,(NAMLST(N),N=1,NLST)
 13         FORMAT(1X,'Elev=',F5.1,15A8)
         ELSE
            WRITE(6,15)TANG,(NAMLST(N),N=1,NLST)
 15         FORMAT(1X,'Azim=',F5.1,15A8)
         END IF

         DO 20 I=IRB,IRE,ISKIP
c            RNGE=RNG(I,1)
            RNGE=R0+(I-1)*DROLD
            
C           EXTRACT FIELD(S) FOR PLOTTING AS A FUNCTION OF RANGE
C
            HRNG=RNGE*COSE
            Z=H0+SINE*RNGE+HRNG*HRNG/RE
            IF(Z.LT.PZMN .OR. Z.GT.PZMX)GO TO 20
            IF(RNGE.GE.PRMN .AND. RNGE.LE.PRMX)THEN
               DO N=1,NLST
                  IFL=IFIND(LSTNAM(N),NAMFLD,MXF)
                  IF(IFL.NE.0)FLDAT(N)=DAT(I,J,IFL)
               END DO
               WRITE(6,17)RNGE,(FLDAT(N),N=1,NLST)
 17            FORMAT(1X,'   R=',F8.3,15F8.2)
            END IF

 20      CONTINUE
 30   CONTINUE

      RETURN
      END
