c
c----------------------------------------------------------------------X
c
      SUBROUTINE FLDIDFF(NAMFLD,NFLDS,IFLD)
C
C  DETERMINE IF REQUESTED FIELDS ARE ON THE INPUT FIELD FORMAT TAPE
C
C  FIELD-CALIBRATED ENGINEERING UNITS - LOWER 4 BITS (RP-7)
C        DBZ   - RADAR REFLECTIVITY (DB REL TO MM6/M3)
C        VF    - RADIAL VELOCITY (M/S)
C        SW    - SPECTRAL WIDTH (M/S)
C        SNR   - SIGNAL TO NOISE RATIO (DB)
C        NCP   - NORMALIZED COHERENT POWER (NON-DIMENSIONAL: 0,1)
C     UNSCALED COUNTS - UPPER 4 BITS (RP-6 AND RP-7)
C        DZDM  - REFLECTIVITY (DBZ) FROM PRIMARY RECEIVED POWER (DBM)
C        DZSH  - REFLECTIVITY (DBZ) FROM S-BAND, HORIZONTAL POWER (DBM)
C        DBM   - PRIMARY RECEIVED POWER (DB RELATIVE TO MW)
C        ZDR   - RATIO OF HORIZ AND VERT PRIMARY REFLECTIVITIES (DB)
C        DZXH  - REFLECTIVITY FROM X-BAND RECEIVED POWER (HORIZONTAL)
C        DZXV  - REFLECTIVITY FROM X-BAND RECEIVED POWER (TRANSMITTED
C                HORIZONTAL AND RECEIVED VERTICAL, LDRX=DZXH-DZXV)
C        VEL   - RADIAL VELOCITY FROM FREQUENCY COUNTS (HORIZONTAL)
C        VELV  - RADIAL VELOCITY FROM FREQUENCY COUNTS (VERTICAL)
C        SPECW - SPECTRAL WIDTH FROM FREQUENCY COUNTS (HORIZONTAL)
C
C
C  INPUT:
C        NAMFLD - NAME OF FIELD REQUESTED
C        NFLDS  - TOTAL NUMBER OF FIELDS REQUESTED
C  OUTPUT:
C        IFLD   - FIELD NUMBER ON TAPE (#) OF THE REQUESTED FIELD
C                 ( 0) IF THE REQUESTED FIELD IS NOT ON THE INPUT TAPE
C                 ( #) IF THE REQUESTED FIELD IS ON THE INPUT TAPE
C                 (-1) IF THE REQUESTED FIELD IS TO BE 'SWATHED'
C                 (-2) IF THE REQUESTED FIELD IS TO BE INTEGRATED
C                 (-3) IF THE REQUESTED FIELD IS THE INTEGRATION TIME
C                 (-4) IF THE REQUESTED FIELD IS EXTRACTED AS PSEUDO-RHI
C                 (-5) IF THE REQUESTED FIELD IS TO BE AVERAGED SWEEP-TO-SWEEP
C                 (-6) IF THE REQUESTED FIELD IS THE NUMBER OF SWEEPS AVERAGED
C                      SEE ROUTINES SWATH, INTEGR, ISOCHRON, FXSWATH

      DIMENSION IFLD(NFLDS)
      CHARACTER*8 FLDNAM,NAMFLD(NFLDS)
      CHARACTER*1 POUND

      WRITE(6,11)NFLDS
   11 FORMAT(1X,/,' Fields to be processed = ',I3,
     +       /,1X,' IFLD: Number associated with the requested field',
     +       /,1X,'       ( #) Number of field on the input tape    ',
     +       /,1X,'       ( 0) Field is derived                     ',
     +       /,1X,'       (-1)   "    " to be swathed               ',
     +       /,1X,'       (-2)   "    " to be integrated in time    ',
     +       /,1X,'       (-3)   "    " the integration time        ',
     +       /,1x,'       (-4)   "    " to be extracted pseudo-rhi  ',
     +       /,1x,'       (-5)   "    " to be averaged sweep-to-sweep',
     +       /,1x,'       (-6)   "    " is the number averaged'/)

      DO 100 I=1,NFLDS
         IF(IFLD(I).LE.-1)GO TO 90
         IFLD(I)=0

C        FIELD-CALIBRATED ENGINEERING UNITS - LOWER 4 BITS
C
         IF(NAMFLD(I).EQ.'DBZ     ')IFLD(I)=1
         IF(NAMFLD(I).EQ.'VF      ')IFLD(I)=2
         IF(NAMFLD(I).EQ.'SW      ')IFLD(I)=3
         IF(NAMFLD(I).EQ.'SNR     ')IFLD(I)=4
         IF(NAMFLD(I).EQ.'NCP     ')IFLD(I)=6

C        UNSCALED COUNTS - UPPER 4 BITS
C
         IF(NAMFLD(I).EQ.'DBM     ')IFLD(I)=16
         IF(NAMFLD(I).EQ.'DZDM    ')IFLD(I)=16
         IF(NAMFLD(I).EQ.'DZSH    ')IFLD(I)=16
         IF(NAMFLD(I).EQ.'ZDR     ')IFLD(I)=32
         IF(NAMFLD(I).EQ.'DBMXH   ')IFLD(I)=96
         IF(NAMFLD(I).EQ.'DZXH    ')IFLD(I)=96
         IF(NAMFLD(I).EQ.'DBMXV   ')IFLD(I)=112
         IF(NAMFLD(I).EQ.'DZXV    ')IFLD(I)=112
         IF(NAMFLD(I).EQ.'DBMXVUF ')IFLD(I)=112
         IF(NAMFLD(I).EQ.'DZXVUF  ')IFLD(I)=112
         IF(NAMFLD(I).EQ.'VEL     ')IFLD(I)=128
         IF(NAMFLD(I).EQ.'VELV    ')IFLD(I)=160
         IF(NAMFLD(I).EQ.'SPECW   ')IFLD(I)=208
         IF(NAMFLD(I).EQ.'SPECW_B ')IFLD(I)=208

C        SPECIAL MNEMONIC (# FOLLOWED BY FIELD INDEX NUMBER) 
C        TO PLOT RECORDED PROCESSOR COUNTS.  SEE RDFF.
C
         IF(IFLD(I).EQ.0)THEN
            WRITE(FLDNAM,83)NAMFLD(I)
   83       FORMAT(A8)
            READ(FLDNAM,85)POUND
   85       FORMAT(A1)
            IF(POUND.EQ.'#')THEN
               READ(FLDNAM,87)INDX
   87          FORMAT(1X,I3)
               IFLD(I)=INDX
            ELSE
               INDX=0
            END IF
         END IF

   90    CONTINUE
         WRITE(6,93)I,NAMFLD(I),IFLD(I)
   93    FORMAT(1X,'     #',I2,' - ',A8,' IFLD = ',I3)
  100 CONTINUE
      WRITE(6,101)
  101 FORMAT(1X,/)
      RETURN
      END


