c
c----------------------------------------------------------------------X
c
      SUBROUTINE SETBCKGRND(BGFLAG)

C  SET BACKGROUND COLOR
C        IF (BGFLAG.EQ.'B') USE BLACK BACKGROUND, WHITE FOREGROUND
C           (BGFLAG.EQ.'W')  "  WHITE      "    , BLACK     "
C     BACKGROUND (COLOR INDEX 0):  BLACK - CALL GSCR(1,0,0.,0.,0.)
C                                  WHITE - CALL GSCR(1,0,1.,1.,1.)
C     FOREGROUND (COLOR INDEX 1):  BLACK - CALL GSCR(1,1,0.,0.,0.)
C                                  WHITE - CALL GSCR(1,1,1.,1.,1.)
C     OTHER FORE/BACKGROUNDS
C     FOREGROUND ALWAYS BLACK      BLACK - CALL GSCR(1,1,0.,0.,0.)
C     FOREGROUND ALWAYS WHITE      WHITE - CALL GSCR(1,0,1.,1.,1.)
C     OTHER BACKGROUNDS (INDEX 0): CYAN  - CALL GSCR(1,0,0.0,1.0,1.0)
C                                  SBLUE - CALL GSCR(1,0,0.529,0.808,0.98)
C                                  SBLUE - CALL GSCR(1,0,0.69,0.886,1.0)
C                                  LCYAN - CALL GSCR(1,0,0.88,1.0,1.0)
C                                  GRAY  - CALL GSCR(1,0,0.8,0.8,0.8)
C                                  NBLUE - CALL GSCR(1,0,0.0,0.0,0.5)
      INCLUDE 'colors.inc'
      CHARACTER*1 BGFLAG

c-----print *,'In SETBCKGRND, BGFLAG=',BGFLAG
      CALL SFLUSH
      IF(BGFLAG.EQ.'W')THEN
         CALL GSCR(1,0,1.,1.,1.)
         CALL GSCR(1,1,0.,0.,0.)
      ELSEIF(BGFLAG.EQ.'C')THEN
         CALL GSCR(1,0,0.0,1.0,1.0)
         CALL GSCR(1,1,0.,0.,0.)
      ELSEIF(BGFLAG.EQ.'S')THEN
c         CALL GSCR(1,0,0.529,0.808,0.98)
         CALL GSCR(1,0,0.69,0.886,1.0)
         CALL GSCR(1,1,0.,0.,0.)
      ELSEIF(BGFLAG.EQ.'L')THEN
         CALL GSCR(1,0,0.88,1.0,1.0)
         CALL GSCR(1,1,0.,0.,0.)
      ELSEIF(BGFLAG.EQ.'G')THEN
         CALL GSCR(1,0,0.8,0.8,0.8)
         CALL GSCR(1,1,0.,0.,0.)
      ELSEIF(BGFLAG.EQ.'B')THEN
         CALL GSCR(1,0,0.,0.,0.)
         CALL GSCR(1,1,1.,1.,1.)
      ELSEIF(BGFLAG.EQ.'N')THEN
         CALL GSCR(1,0,0.0,0.0,0.5)
         CALL GSCR(1,1,1.,1.,1.)
      END IF

      RETURN
      END

