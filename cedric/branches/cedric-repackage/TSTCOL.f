c
c----------------------------------------------------------------------X
c
      SUBROUTINE TSTCOL
C
C     Routines to plot frames showing color tables
C
      INCLUDE 'colors.inc'
      CHARACTER LABL*60

      print *,'TSTCOL: colors=',iwhite,iblack,ired,igreen,iblue
      FYB=0.085

C  RAINBOW COLORS WITH BLACK BACKGROUND
C
c      WRITE(6,11)
 11   FORMAT(1X,/,'RAINBOW: BLACK BACK (0=000); WHITE FORE (1=111)')
      CALL DFCLRS(0)
      CALL DFCLRS(3)
      LC=0
      CALL PLTCOL(LC)
      CALL SET (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,0)
      LABL='RAINBOW: BACKGROUND (0-BLACK) AND FOREGROUND (1-WHITE)'
      CALL PLCHMQ(0.05,0.05,LABL,15.0,0.0,-1.0)
      CALL GRDCOL(FYB)

C  RAINBOW COLORS WITH WHITE BACKGROUND
C
c      WRITE(6,13)
 13   FORMAT(1X,/,'RAINBOW: WHITE BACK (0=000); BLACK FORE (1=111)')
      CALL DFCLRS(0)
      CALL DFCLRS(2)
      LC=1
      CALL PLTCOL(LC)
      CALL SET (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,0)
      LABL='RAINBOW: BACKGROUND (0-WHITE) AND FOREGROUND (1-BLACK)'
      CALL PLCHMQ(0.05,0.05,LABL,15.0,0.0,-1.0)
      CALL GRDCOL(FYB)

C  GRAYTONE COLORS WITH WHITE BACKGROUND
C
c      WRITE(6,15)
 15   FORMAT(1X,/,'GRAYTONE: WHITE BACK (0=111); BLACK FORE (1=000)')
      CALL DFCLRS(1)
      LC=1
      CALL PLTCOL(LC)
      CALL SET (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,0)
      LABL='GRAYTONE: BACKGROUND (0-WHITE) AND FOREGROUND (1-BLACK)'
      CALL PLCHMQ(0.05,0.05,LABL,15.0,0.0,-1.0)
      CALL GRDCOL(FYB)

C     Restore rainbow color table
C
      CALL DFCLRS(0)
      CALL GSPLCI(1)
      CALL GSTXCI(1)
      RETURN
      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE PLTCOL(LC)
C
C  PLOT ICMX COLORS, INCLUDING BACKGROUND (IC=0) AND FOREGROUND (IC=1)
C  IN X:I=10 X Y:J=JMX BOXES.  EACH BOX HAS DIMENSIONS 0.1 X 0.1
C  THE RGB FRACTIONS ARE SET FOR COLOR INDICES 2,3,...,63 IN ROUTINE
C  DFCLRS.  FOR GRAYTONES REVERSE THE COLOR TABLE FOR LABELING INDEX.
C
      INCLUDE 'colors.inc'
      CHARACTER LABL*2,LABC*4
      DIMENSION X(5),Y(5)
      DATA JMX,ICMX/9,65/

      CALL SET(0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
      DO 60 J=1,JMX
         K=11-J
         Y(1)=0.1*K
         Y(2)=0.1*K
         Y(3)=0.1*(K-1)
         Y(4)=0.1*(K-1)
         Y(5)=0.1*K
         DO 50 I=1,10
            IC=I-1+10*(J-1)
            JC=IC
            X(1)=0.1*(I-1)
            X(2)=0.1*I
            X(3)=0.1*I
            X(4)=0.1*(I-1)
            X(5)=0.1*(I-1)
            CALL SFLUSH
            IF(IC.LE.ICMX)THEN
               CALL FILCOL(JC,X,Y,5)
c            ELSE
c               CALL FILDOT(JC,X,Y,5)
            END IF

            XP=0.5*(X(1)+X(2))
            YP=0.5*(Y(2)+Y(3))
            CALL SFLUSH
            IF(JC.EQ.0)THEN
               WRITE(LABL,11)
 11            FORMAT(' B')
               CALL GSPLCI(1)
            ELSE IF(JC.EQ.1)THEN
               WRITE(LABL,13)
 13            FORMAT(' F')
               CALL GSPLCI(0)
            ELSE IF(JC.GE.2.AND.JC.LE.62)THEN
               KC=64-JC
               WRITE(LABL,15)JC
 15            FORMAT(I2)
               CALL GSPLCI (KC)
            ELSE IF(JC.GE.63.AND.JC.LE.65)THEN
               WRITE(LABL,15)JC
               CALL GSPLCI (LC)
            ELSE IF(JC.EQ.66)THEN
               WRITE(LABL,17)
 17            FORMAT('Bl')
               CALL GSPLCI(IWHITE)
            ELSE IF(JC.EQ.67)THEN
               WRITE(LABL,19)
 19            FORMAT('Wh')
               CALL GSPLCI(IBLACK)
            ELSE IF(JC.EQ.68)THEN
               WRITE(LABL,21)
 21            FORMAT('Gy')
               CALL GSPLCI(IBLACK)
            ELSE IF(JC.EQ.69)THEN
               WRITE(LABL,23)
 23            FORMAT('Rd')
               CALL GSPLCI(IBLACK)
            ELSE IF(JC.EQ.70)THEN
               WRITE(LABL,25)
 25            FORMAT('Gn')
               CALL GSPLCI(IBLACK)
            ELSE IF(JC.EQ.71)THEN
               WRITE(LABL,27)
 27            FORMAT('Bu')
               CALL GSPLCI(IWHITE)
            ELSE IF(JC.EQ.72)THEN
               WRITE(LABL,29)
 29            FORMAT('Cy')
               CALL GSPLCI(IBLACK)
            ELSE IF(JC.EQ.73)THEN
               WRITE(LABL,31)
 31            FORMAT('Mg')
               CALL GSPLCI(IBLACK)
            ELSE IF(JC.EQ.74)THEN
               WRITE(LABL,33)
 33            FORMAT('Ye')
               CALL GSPLCI(IBLACK)
            ELSE IF(JC.EQ.75)THEN
               WRITE(LABL,35)
 35            FORMAT('Bm')
               CALL GSPLCI(IWHITE)
            ELSE IF(JC.EQ.76)THEN
               WRITE(LABL,37)
 37            FORMAT('Sb')
               CALL GSPLCI(IWHITE)
            ELSE IF(JC.EQ.77)THEN
               WRITE(LABL,39)
 39            FORMAT('Or')
               CALL GSPLCI(IBLACK)
            ELSE IF(JC.EQ.78)THEN
               WRITE(LABL,41)
 41            FORMAT('Fg')
               CALL GSPLCI(IWHITE)
            END IF
            IF(JC.LE.ICMX)CALL PLCHMQ(XP,YP,LABL,20.0,0.0,0.0)
 50      CONTINUE
 60   CONTINUE
C
C     RESTORE FOREGROUND AS COLOR FOR LABELING
C
 100  CALL GSPLCI(1)
      RETURN
      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE FILCOL(ICOLOR,XP,YP,IPTS)
C
C  GKS CALLS TO FILL POLYGONS WITH COLOR INDEX
C
      DIMENSION XP(*),YP(*)

      XP(IPTS)=XP(1)
      YP(IPTS)=YP(1)
c      print *,'FILCOL: icolor=',icolor
      CALL GSFACI (ICOLOR)
      CALL GFA (IPTS,XP,YP)
      RETURN
      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE FILDOT(IDOT,XP,YP,IPTS)
C
C  CALLS TO FILL POLYGONS WITH DOT PATTERNS
C
      DIMENSION XP(*),YP(*),DST(10),IND(20)
      DIMENSION ID1(8,8),ID2(8,8),ID3(8,8),ID4(8,8)
      INCLUDE 'colors.inc'
C
C  Define four different dot patterns
C
      DATA ID1/0,0,0,0,0,0,0,0,
     +         0,0,0,0,0,0,0,0,
     +         0,0,1,1,1,1,0,0,
     +         0,0,1,1,1,1,0,0,
     +         0,0,1,1,1,1,0,0,
     +         0,0,1,1,1,1,0,0,
     +         0,0,0,0,0,0,0,0,
     +         0,0,0,0,0,0,0,0/

      DATA ID2/0,1,0,1,0,1,0,1,
     +         1,0,1,0,1,0,1,0,
     +         0,1,0,1,0,1,0,1,
     +         1,0,1,0,1,0,1,0,
     +         0,1,0,1,0,1,0,1,
     +         1,0,1,0,1,0,1,0,
     +         0,1,0,1,0,1,0,1,
     +         1,0,1,0,1,0,1,0/

      DATA ID3/1,1,1,1,1,1,1,1,
     +         1,1,1,1,1,1,1,1,
     +         1,1,1,1,1,1,1,1,
     +         1,1,1,1,1,1,1,1,
     +         1,1,1,1,1,1,1,1,
     +         1,1,1,1,1,1,1,1,
     +         1,1,1,1,1,1,1,1,
     +         1,1,1,1,1,1,1,1/
      
C  Set line (GSPLCI) and text (GSTXCI) colors.
C     Foreground = (1)
C  Set the sizes of the dot and characters.
C
      CALL GSMKSC (1000.)

C  Fill the polygon with a specified dot pattern
C
      XP(IPTS)=XP(1)
      YP(IPTS)=YP(1)
      IF(IDOT.EQ.79)THEN
         CALL GSPLCI(IRED)
         CALL GSTXCI(IRED)
         CALL SFSETP (ID1)
         CALL SFSETI ('DO - DOT-FILL FLAG',0)
         CALL SFSETI ('CH - CHARACTER SPECIFIER',0)
         CALL SFSETR ('SP - SPACING OF FILL LINES',0.012)
         CALL SFSETI ('AN - ANGLE OF FILL LINES',45)
         CALL SFWRLD (XP,YP,IPTS,DST,100,IND,102)
         CALL SFSETI ('AN - ANGLE OF FILL LINES',-45)
         CALL SFNORM (XP,YP,IPTS,DST,100,IND,102)
      ELSE IF(IDOT.EQ.80)THEN
         CALL GSPLCI(1)
         CALL GSTXCI(1)
         CALL GSCHH (.002)
         CALL SFSETP (ID1)
         CALL SFSETI ('DO - DOT-FILL FLAG',1)
C         CALL SFSETI ('CH - CHARACTER SPECIFIER',0)
         CALL SFSETC ('CH - CHARACTER SPECIFIER','x')
         CALL SFSETR ('SP - SPACING OF FILL LINES',0.004)
         CALL SFSETI ('AN - ANGLE OF FILL LINES',0)
         CALL SFWRLD (XP,YP,IPTS,DST,100,IND,102)
      ELSE IF(IDOT.EQ.81)THEN
         CALL GSPLCI(IBLUE)
         CALL GSTXCI(IBLUE)
         CALL GSCHH (.004)
         CALL SFSETP (ID3)
         CALL SFSETI ('DO - DOT-FILL FLAG',1)
         CALL SFSETC ('CH - CHARACTER SPECIFIER','O')
         CALL SFSETR ('SP - SPACING OF FILL LINES',0.008)
         CALL SFSETI ('AN - ANGLE OF FILL LINES',0)
         CALL SFWRLD (XP,YP,IPTS,DST,100,IND,102)
      END IF
      RETURN
      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE GRDCOL(FYB)
C
C  ROUTINE TO ADD FRAME OF LABELING GRID
C
      CHARACTER LABLS*3

      CALL SFLUSH
      CALL SET(0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
      CALL PERIM(1,1,1,1)
      Y1=0.1
      Y2=1.0
      DO 10 I=1,9
         X1=0.1*I
         X2=0.1*I
         CALL LINE(X1,Y1,X2,Y2)
   10 CONTINUE
      X1=0.0
      X2=1.0
      DO 20 I=1,9
         Y1=0.1*I
         Y2=0.1*I
         CALL LINE(X1,Y1,X2,Y2)
   20 CONTINUE
      CALL MYFRAME
      RETURN
      END
