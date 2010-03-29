      SUBROUTINE PROFIL(KRD)
C
C        FILLS COMMON /CPRO/
C        FORMAT OF PROCESS COMMAND
C
C
C   VARIABLE   DESCRIPTION                  FIELD   NOTES
C   --------   -----------                  -----   -----
C
C   KOMM       'PRO'                         P1     COMMAND
C   KDAY       DATE OF DATA                  P2     YYMMDD
C   KBTIM      BEGINNING TIME TO PROCESS     P3     HHMMSS
C   KETIM      ENDING TIME TO PROCESS        P4     HHMMSS
C   IROV       'RUNOVER' OR 'APPEND'         P5
C   ISWUSR     'NUMBER' OR 'FIXED'           P6
C   FXSTOL     ANGLE TOLERANCE               P7     Degrees
C   TRANSIT    'YES AFT', 'YES BEF', or 'NO' P9     Transition flag
C   TABLE      Use/do not use sweep table   P10
C
C     RUNOVER - merge the current volume scan with a new volume scan
C               Requires a new INPUT command specifying the new file
C               to be merged.  The INPUT command must immediately 
C               follow the PROCESS command.  RUNOVER would be used for
C               merging several DORADE format sweep files.
C     APPEND  - merge the next volume scan with the current one.  The
C               scans to be merged reside in the same disk file so no
C               new INPUT command is required.
C     COMBINE - Combines RUNOVER and APPEND functionalities.
C
C     FXSTOL  - When using the fixed angle (ISWUSR = 'F'IXED) to delineate 
C               scans, all beams within FXSTOL of the first beam in the 
C               sequence belong within the same sweep regardless of sweep 
C               number.  When using the sweep number (ISWUSR = 'N'UMBER) 
C               to delineate scans, beams with the same sweep number are 
C               processed together and their angular positions are assigned 
C               to the nominal fixed angle, regardless of their actual position.
C     ELTUS   - Acceptable departure of actual "fixed" angle from the nominal
C               fixed angle.  Beams with |Fixed - Actual| > ELTUS are tossed,
C               where Actual = (PPI) elevation angle, or (RHI) azimuth angle.
C               ELTUS is specified with the RESET command (see RNGFIL) and is
C               passed around with the /CRNG/ common block.
C
      PARAMETER (MAXSKP=27)
      CHARACTER*8 KRD(10)

      COMMON /CPRO/KDAY,KBTIM,KETIM,IROV,ISWMTH,FXSTOL
      COMMON /CPROC/ IREORD(3)
      CHARACTER*1 IREORD

      COMMON /FXTABL/ IFXTAB,ISKIP,IACCPT,FTABLE(MAXSKP),ITRAN
      COMMON /FORMAT/ IRP,IBLOCK
C
      CHARACTER ISWTAB(2)*12 
      CHARACTER*1 IRTAB(4), ITRN, ISWUSR
      CHARACTER*8 TABLE, TABOUT, TRANSIT, IRPRNT(4)
      CHARACTER*46 TRANOUT
      DATA ISWTAB/'SWEEP NUMBER', 'FIXED ANGLE'/
      DATA IRTAB/'A','N','R','C'/
      DATA IRPRNT/'APPEND','NONE','RUNOVER','COMBINE'/
      
C
      READ (KRD,101)TMPDAY,TBTIM,TETIM,ITRN,ISWUSR,FXSTOL,TRANSIT,TABLE
 101  FORMAT(/F8.0/F8.0/F8.0/A1,7X/A1,7X/F8.0//A8/A8)
C
      KDAY = TMPDAY
      KBTIM = TBTIM
      KETIM = TETIM
C
      IF (KBTIM.LE.0) KBTIM=0
      IF (KETIM.LE.0) KETIM=240000
C
C     Check option for combining volume scans:
C     IROV: (-1) APPEND, (0) NONE, (1) RUNOVER, (2) COMBINE
C           (0) NONE ==> Normal processing
C
      I = LOCATEC(ITRN,IRTAB,4)
      IF (I.LE.0) I=2
      IROV = I-2
      IF(ISWUSR.EQ.'F') THEN
C
C        FIXED ANGLE USED TO DETERMINE SWEEPS
C
         ISWMTH=1
         FXSTOL=AMAX1(FXSTOL,0.001)
      ELSE IF (IRP.NE.1) THEN
C
C        SWEEP NUMBER USED INSTEAD
C
         ISWMTH=0
         FXSTOL=0.001
      ELSE
         WRITE(*,880)
 880     FORMAT(//,'***INVALID SWPMOD FOR DATA FORMAT')
         STOP
      END IF
C
C     SEE IF USER WANTS TO DISCARD BEAMS FLAGGED AS TRANSITIONS
C
      IF (TRANSIT(1:1).EQ.'Y') THEN
         ITRAN=2
         TRANOUT='YES, BEFORE ANY HOUSEKEEPING IN BEAM IS NOTED'
         IF (TRANSIT.EQ.'YES AFT') THEN
            ITRAN=1
            TRANOUT='YES, AFTER SOME HOUSEKEEPING IN BEAM IS NOTED'
         END IF
      ELSE
         ITRAN=0
         TRANOUT='NO'
      END IF
C
C     SEE IF USER WANTS TO USE A PRE-DEFINED SWEEP TABLE TO DISCARD SOME SWEEPS
C
      IF (TABLE.EQ.'FXTABLE') THEN
         IFXTAB=1
         TABOUT='USED'
      ELSE
         IFXTAB=0
         TABOUT='NOT USED'
      END IF
      
      PRINT 885
885   FORMAT(//5X,'SUMMARY OF PROCESS COMMAND ')
      PRINT 887
887   FORMAT(5X,'------- -- ------- ------- ')
      PRINT 888,KDAY,KBTIM,KETIM,IRPRNT(I),ISWTAB(ISWMTH+1),TRANOUT,
     X     TABOUT
888   FORMAT(/5X,'                        DATE (YYMMDD): ',I6.6,
     X       /5X,'              BEGINNING TIME (HHMMSS): ',I6,
     X       /5X,'                 ENDING TIME (HHMMSS): ',I6,
     X       /5X,'  SPECIAL VOLUME PROCESSING PERFORMED: ',A8,
     X       /5X,'          SCANS WILL BE DETERMINED BY: ',A12,
     X       /5X,' DISCARD BEAMS FLAGGED AS TRANSITIONS: ',A46,
     X       /5X,'            USER SUPPLIED SWEEP TABLE: ',A8)
      IF(ISWMTH.EQ.1) THEN
         PRINT 889, FXSTOL
889      FORMAT(5X,'          FIXED ANGLE TOLERANCE (DEG): ',F8.2)
         PRINT *,'       ANGLES THAT ARE WITHIN TOLERANCE OF THE FIRST'
         PRINT *,'       BEAM IN A SEQUENCE WILL BE BUNDLED TOGETHER AS'
         PRINT *,'       A CONTINUOUS SWEEP, IGNORING THE SWEEP NUMBER.'
      END IF
C
      IF (KDAY.LE.0) CALL TPQERX(326,1)
      RETURN
 105  FORMAT (5X,'***ERROR IN PROCESS COMMAND  ',A10,'=',I10)
      END
