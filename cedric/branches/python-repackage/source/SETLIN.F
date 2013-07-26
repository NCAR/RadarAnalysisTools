c
c----------------------------------------------------------------------X
c
      SUBROUTINE SETLIN(IGRPLT,BGFLAG,IBLACK,IWHITE,JLW)
C
C     SET LINE WIDTH AND FOREGROUND (LINES AND TEXT) COLOR.

C     BACKGROUND (COLOR INDEX 0): BLACK - CALL GSCR(1,0,0.,0.,0.)
C     FOREGROUND (COLOR INDEX 1): WHITE - CALL GSCR(1,1,1.,1.,1.)
C     OR
C     BACKGROUND (COLOR INDEX 0): WHITE - CALL GSCR(1,0,1.,1.,1.)
C     FOREGROUND (COLOR INDEX 1): BLACK - CALL GSCR(1,1,0.,0.,0.)

C     Note: Cannot call gscr to set background once drawing has started.
C           Call gscr to set foreground then call gsplci and gstxci with
C           index (1); otherwise, white lines will be dashed-looking.
C
      CHARACTER*1 BGFLAG
      DATA ILW/1200/

      CALL SFLUSH
      IF(IGRPLT.NE.1)THEN

         IF(BGFLAG.EQ.'W')THEN

C           background (white), foreground (black)
C
            CALL GSCR(1,1,0.,0.,0.)
            CALL SETUSV('LW',JLW)
         ELSE

C           background (black), foreground (white)
C
            CALL GSCR(1,1,1.,1.,1.)
c            CALL SETUSV('LW',ILW)
            CALL SETUSV('LW',JLW)
         END IF
      ELSE

C        background (white), foreground (black)
C
         CALL GSCR(1,1,0.,0.,0.)
         CALL SETUSV('LW',JLW)
      END IF
      CALL GSPLCI(1)
      CALL GSTXCI(1)
      RETURN
      END


