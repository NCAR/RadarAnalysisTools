c
c----------------------------------------------------------------------X
c
      SUBROUTINE SAVSCAN(INDAT,NSCAN)
C
C  SAVE VARIABLES FOR PLOTTING FIXED ANGLE AS A FUNCTION OF SCANNING ANGLE
C     WITHIN THE CURRENT SWEEP
C
C     PAMN,PAMX - MINIMUM AND MAXIMUM ANGLES (DEG) FOR PLOT BOUNDARIES
C     AFMN,AFMX -    "     "     "    FIELD VALUE   "    "       "
C                 (1) FIXED ANGLE, (2) FIXED ANGLE ERROR, (3) SCAN INCREMENT
C         NSCAN - FLAG (1) TO TURN ON PLTSCAN
C
      CHARACTER*8 INDAT(10)
      COMMON/PLSCANS/PAMN,PAMX,AFMN(3),AFMX(3)

      WRITE(6,11)(INDAT(I),I=2,10)
   11 FORMAT(1X,'PSCAN: ',9A8)
      READ(INDAT,13)PAMN,PAMX,AFMN(1),AFMX(1),AFMN(2),AFMX(2),
     +                        AFMN(3),AFMX(3)
   13 FORMAT(/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0)
      NSCAN=1
      RETURN
      END
