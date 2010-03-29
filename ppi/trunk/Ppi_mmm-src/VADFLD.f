c
c----------------------------------------------------------------------X
c
      SUBROUTINE VADFLD(IOUT,NIN1,NIN2,NPRNT,H0)
C
C  FUNCTION - COMPUTE VAD RADIAL VELOCITY FIELD FROM FOURIER COEFFICIENTS
C             PREVIOUSLY COMPUTED (VAD.f) AND STORED IN COMMON/VADWINDS/
C             F(I,J,OUT)=VRVAD(I,J,IIN1)
C
C     IOUT   - OUTPUT FIELD NUMBER (CONTAINS VAD WINDS USING COEF)
C     NIN1   - NAME TO BE ASSOCIATED WITH PREVIOUS VAD ANALYSIS
C     NIN2   - IF 'REG', USE SWATH GRID; OTHERWISE USE INPUT POSITIONS.
C     NPRNT  - PRINT FLAG FOR VAD WINDS ('    ') NO, ('PRNT') YES,
C              ('FILE') YES and COEFFICIENTS TO ASCII FILE (fort.999).
C     NAMFLD - NAME LIST OF ALL FUNCTION-DERIVED FIELDS
C     NAMVD  -   "    "   " ONLY   VAD-DERIVED     "
C     MVD    - NUMBER OF VAD-DERIVED FIELDS (MXVD=10)
C
C  VAD MEAN OUTPUT QUANTITIES STORED IN COMMON/VADWINDS/:
C     U0,V0  - HORIZONTAL WINDS       FOR THE ITH RANGE GATE
C     SPD    -     "      WIND SPEED   "   "   "    "     "
C     DIR    -     "       "   DIREC   "   "   "    "     "
C     CON    -     "      CONVERGENCE  "   "   "    "     "
C     WVD    - VERTICAL WIND (CON)     "   "   "    "     "
C     STR    - STRETCHING DEFORMATION  "   "   "    "     "
C     SHR    - SHEARING        "       "   "   "    "     "
C     ERR    - RMS DIFFERENCE BETWEEN FOURIER FIT AND INPUT RADIAL VEL
C     DBZ    - MEAN REFLECTIVITY FACTOR FOR THE ITH RANGE GATE
C     U_VD   - AMOUNT TO SUBTRACT FROM U-COMPONENT
C     V_VD   -    "    "     "      "  V    "
C     AZMVD  - AZIMUTH OF POSITIVE U DIRECTION
C
      INCLUDE 'dim.inc'
      INCLUDE 'data.inc'
      INCLUDE 'input.inc'
      INCLUDE 'swth.inc'
      INCLUDE 'vadwinds.inc'

      COMMON /INPUTCH/NAMFLD(MXF),IRATYP,ICORD
      CHARACTER*8 NAMFLD,IRATYP,ICORD
      CHARACTER*8 NIN1,NIN2,NPRNT
      CHARACTER LINE*130

      DATA TORAD,TODEG,PI/0.017453293,57.29577951,3.141592654/
      DATA EPS/1.0E-4/

      LOGICAL PLTSW

      IF(FXOLD.GT.60.0)THEN
         WRITE(6,5)FXOLD
 5       FORMAT(1X,'*** NO VAD FLD ANALYSIS: E> ',F6.2,' DEG ***')
         RETURN
      END IF
      SINE=SIN(TORAD*FXOLD)
      COSE=COS(TORAD*FXOLD)

C     SET THE TYPE OF OUTPUT FIELD (Swath'd or otherwise)
C
      IF(NIN2.EQ.'REG')THEN
         ISW=2
         PLTSW=.TRUE.
         GSPC=DRSW
         IFLD(IOUT)=-1
      ELSE
         ISW=1
         PLTSW=.FALSE.
         GSPC=DROLD
         IFLD(IOUT)=0
      END IF

C  FIND INDEX FOR THE PREVIOUSLY-ANALYZED VAD FIELD NAME ASSOCIATED
C  WITH COEFFICIENTS AND THE INDEX FOR CURRENT OUTPUT VAD FIELD
C
      LVD=IFIND(NAMFLD(IOUT),NAMVD,MXVD)
      write(*,*)"VADFLD: ",namfld(iout),nin1,nin2,pltsw,nprnt,lvd

      MING=MAX0(MNGATE,1)
      MAXG=MIN0(MXGATE,MXG,MXR)
      DO K=1,2
         READ(999,7)LINE
 7       FORMAT(A130)
         WRITE(6,9)LINE
 9       FORMAT(A130)
      END DO
      DO I=1,MAXG
         READ(999,11)KVD,J,RNGE,Z,AVAD0(I,LVD),AVAD(I,LVD,1),
     +        AVAD(I,LVD,2),BVAD(I,LVD,1),BVAD(I,LVD,2),
     +        U0(I,LVD),V0(I,LVD),SPD(I,LVD),DIR(I,LVD),
     +        CON(I,LVD),STR(I,LVD),SHR(I,LVD),ERR(I,LVD),
     +        DBZ(I,LVD)
 11      FORMAT(2X,I2,I3,F6.2,F6.3,14F8.2)
      END DO

C     LOOP OVER ALL GATES AND ANGLES - COMPUTE VAD FIELD FROM COEFFICIENTS
C
      DO 100 I=MING,MAXG

         IF(RNG(I,ISW).LE.EPS)GO TO 100
         Z=H0+RNG(I,ISW)*SINE
         GAPMX=-999.0
         GAPMN=999.0
         ALFT=BDVAL

         DO 90 J=1,NANG(ISW)
            ANG=AZA(J,ISW)
            IF(ANG.LT.0.0)ANG=ANG+360.0
            ANGR=ANG*TORAD
            IF(AVAD0(I,LVD).NE.BDVAL)THEN
               VRVAD=AVAD0(I,LVD)
               DO K=1,2
                  VRVAD=VRVAD+AVAD(I,LVD,K)*COS(ANGR*K)
     +                 +BVAD(I,LVD,K)*SIN(ANGR*K)
               END DO
            ELSE
               VRVAD=BDVAL
            END IF
            IF(ALFT.EQ.BDVAL)THEN
               ALFT=ANG
            ELSE
               GAP=ABS(ANG-ALFT)
               ALFT=ANG
               IF(GAP.GT.180.0)GAP=ABS(GAP-360.0)
               IF(GAP.LT.GAPMN)GAPMN=GAP
               IF(GAP.GT.GAPMX)GAPMX=GAP
            END IF
            DAT(I,J,IOUT)=VRVAD
 90      CONTINUE

         IF((NPRNT.EQ.'PRNT'.OR.NPRNT.EQ.'FILE').AND.
     +       U0(I,LVD).NE.BDVAL)THEN
            WRITE(6,93)I,RNG(I,ISW),Z,U0(I,LVD),
     +           V0(I,LVD),SPD(I,LVD),DIR(I,LVD),CON(I,LVD),
     +           STR(I,LVD),SHR(I,LVD),ERR(I,LVD),DBZ(I,LVD)
 93         FORMAT(1X,'    IRZ=',I4,2F8.3,'  UVSD=',4F8.2,
     +           '  CTH=',3F8.2,' ERR=',F8.2,' DBZ=',F8.2)
         END IF
         
 100  CONTINUE
      
      RETURN
      END






