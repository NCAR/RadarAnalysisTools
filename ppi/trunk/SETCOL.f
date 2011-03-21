c
c----------------------------------------------------------------------X
c
      SUBROUTINE SETCOL(IGRPLT,BGFLAG,GRAYTYP,GSTR,GRAYEST)

C  GRAYTONE COLOR TABLE  ALWAYS USES WHITE BACKGROUND, BLACK FOREGROUND
C  RAINBOW    "     "   USUALLY   "  BLACK     "     , WHITE     "
C                       BUT MAY USE  WHITE     "     , BLACK     "
C     BACKGROUND (COLOR INDEX 0): BLACK - CALL GSCR(1,0,0.,0.,0.)
C                                 WHITE - CALL GSCR(1,0,1.,1.,1.)
C     FOREGROUND (COLOR INDEX 1): BLACK - CALL GSCR(1,1,0.,0.,0.)
C                                 WHITE - CALL GSCR(1,1,1.,1.,1.)
C  Routine GSCR sets Red-Green-Blue mix for color table.
C     Smallest value of setgray (greatest blackness) = GRAYEST
C     Largest    "    "    "    (    "    whiteness) = GSTR
C     Decrease (increase) GSTR to darken (lighten) all graytones.
C
      INCLUDE 'colors.inc'
      CHARACTER*1 BGFLAG

      CHARACTER*4 GRAYTYP
      DATA PI2,PI2I/1.570796,0.6366198/

C     Special color indices are set in PPI_MMM.f DATA statements
C
c-----DATA IBLACK,IWHITE,IGRAY,IRED,IGREEN,IBLUE/63,64,65,66,67,68/
c-----DATA ICYAN,IMAGENTA,IYELLOW/69,70,71/
c-----DATA IBMAGENT,ISBLUE,IORANGE,IFGREEN,INBLUE/72,73,74,75,76/

C  SET GRAY COLOR TABLE - WHITE BACKGROUND, BLACK FOREGROUND
C     Indices 2-62
C     
      DARKEST=GRAYEST/GSTR
      ANORM = ALOG10(61.0)
      FRLIN  = 1.0 - DARKEST
      FRQUAD = SQRT(FRLIN)
      FRSQRT = FRLIN**2.0
      FRCOS  = PI2I*ACOS(DARKEST)
      FRLOG  = (63.0-10.0**(ANORM*DARKEST))/62.0
      

      print *,'SETCOL: IGRPLT=',igrplt
      IF(IGRPLT.EQ.1)THEN
         BGFLAG='W'
         CALL SETBCKGRND(BGFLAG)

         DO 20 IC=2,62
            
            XIC=(IC-2.0)/60.0

            IF(GRAYTYP.EQ.'LIN ')THEN
               GRAY = 1.0 - FRLIN*XIC
            ELSE IF(GRAYTYP.EQ.'QUAD')THEN
               GRAY = 1.0 - (FRQUAD*XIC)**2.0
            ELSE IF(GRAYTYP.EQ.'SQRT')THEN
               GRAY = 1.0 - (FRSQRT*XIC)**0.5
            ELSE IF (GRAYTYP.EQ.'LOG ')THEN
               GRAY = ALOG10(63.0-FRLOG*IC)/ANORM
            ELSE IF (GRAYTYP.EQ.'COS ')THEN
               GRAY = COS(PI2*FRCOS*XIC)
            END IF
            IF(GRAY.LT.0.0)GRAY=0.0

            R = GSTR*GRAY
            G = GSTR*GRAY
            B = GSTR*GRAY
            CALL GSCR(1,IC,R,G,B)
 20      CONTINUE
      ELSE
         
C  SET RAINBOW COLOR TABLE - BLACK OR WHITE BACKGROUND
C        IF (BGFLAG.EQ.' ') USE BLACK BACKGROUND, WHITE FOREGROUND
C           (BGFLAG.EQ.'W')  "  WHITE      "    , BLACK     "
C
         print *,'SETCOL - CALL SETBCKGRND, bglag=',bgflag
         CALL SETBCKGRND(BGFLAG)

C        DEFINE COLORS ASSOCIATED WITH INDICES IC=2-7
C        Vary R, with G and B fixed
C     
         IC=1
         R=0.8
         G=0.
         B=0.6
         DO 50 I=1,6
            IC=IC+1
            CALL GSCR(1,IC,R,G,B)
            R=R-0.8/7.
 50      CONTINUE

C        DEFINE COLORS ASSOCIATED WITH INDICES IC=8-11
C        Vary B, with R and G fixed
C     
         R=0.0
         G=0.0
         B=0.6
         DO 51 I=1,4
            IC=IC+1
            B=B+.4/5.
            CALL GSCR(1,IC,R,G,B)
 51      CONTINUE

C        DEFINE COLORS ASSOCIATED WITH INDICES IC=12-18
C        Vary G, with R and B fixed
C     
         R=0.0
         B=1.0
         G=0.2
         DO 52 I=1,7
            IC=IC+1
            G=G+0.8/7.
            CALL GSCR(1,IC,R,G,B)
 52      CONTINUE

C        DEFINE COLORS ASSOCIATED WITH INDICES IC=19-32
C        Vary G, with R and B fixed
C     
         R=0.0
         G=0.4
         B=0.0
         DO 53 I=1,14
            IC=IC+1
            CALL GSCR(1,IC,R,G,B)
            G=G+.4/13.
 53      CONTINUE

C        DEFINE COLORS ASSOCIATED WITH INDICES IC=33-40
C        Vary G, with R and B fixed
C     
         R=0.8
         G=0.45
         B=0.0
         DO 54 I=1,7
            IC=IC+1
            CALL GSCR(1,IC,R,G,B)
            G=G+.2/6.
 54      CONTINUE

C        DEFINE COLORS ASSOCIATED WITH INDICES IC=41-46
C        Vary R and G, with B fixed
C     
         R=0.8
         G=0.65
         B=0.0
         DO 55 I=1,7
            IC=IC+1
            G=G+.34/7.
            CALL GSCR(1,IC,R,G,B)
            R=R+0.19/6.
 55      CONTINUE

C        DEFINE COLORS ASSOCIATED WITH INDICES IC=47-55
C        Vary G, with R and B fixed
C     
         R=1.0
         G=0.30
         B=0.0
         DO 56 I=1,8
            IC=IC+1
            CALL GSCR(1,IC,R,G,B)
            G=G+.45/7.
 56      CONTINUE

C        DEFINE COLORS ASSOCIATED WITH INDICES IC=55-61
C        Vary R, with G and B fixed
C     
         R=0.6
         B=0.0
         G=0.0
         DO 57 I=1,7
            IC=IC+1
            CALL GSCR(1,IC,R,G,B)
            R=R+.4/6.
 57      CONTINUE

         IC=IC+1
         CALL GSCR(1,IC,0.8,0.8,0.8)
      END IF

C     Set RGB mix for color table indices 63-76
C     See PPI_MMM.f for indices set in data statement
C
      CALL GSCR(1,IBLACK,  0.0,0.0,0.0)
      CALL GSCR(1,IWHITE,  1.0,1.0,1.0)
      CALL GSCR(1,IGRAY,   0.8,0.8,0.8)
      CALL GSCR(1,IRED,    1.0,0.0,0.0)
      CALL GSCR(1,IGREEN,  0.0,1.0,0.0)
      CALL GSCR(1,IBLUE,   0.0,0.0,1.0)
      CALL GSCR(1,ICYAN,   0.0,1.0,1.0)
      CALL GSCR(1,IMAGENTA,1.0,0.0,1.0)
      CALL GSCR(1,IYELLOW, 1.0,1.0,0.0)
      CALL GSCR(1,IBMAGENT,0.5 ,0.0 ,1.0 )
      CALL GSCR(1,ISBLUE,  0.2 ,0.56,0.8 )
      CALL GSCR(1,IORANGE, 1.0 ,0.5 ,0.0 )
      CALL GSCR(1,IFGREEN, 0.14,0.56,0.14)
      CALL GSCR(1,INBLUE,  0.0,0.0,0.5)
      RETURN
      END
