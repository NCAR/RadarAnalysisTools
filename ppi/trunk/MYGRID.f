c
c----------------------------------------------------------------------X
c
      SUBROUTINE MYGRID(INDAT,NFRAME,BGFLAG)
C
C  ROUTINE TO ADD FRAME OF LABELING GRID
C
      INCLUDE 'colors.inc'
      CHARACTER*8 INDAT(10),JNDAT(10)
      CHARACTER LAB*64,LABLS*3,BGFLAG*1

C     SET BACKGROUND (0) AND FOREGROUND (1) COLORS
C        WHITE IS ALL 1'S; BLACK IS ALL 0'S
C
      BGFLAG=INDAT(2)(1:1)
      print *,'MYGRID: bckgrnd=',bckgrnd
      CALL SETBCKGRND(BGFLAG)
      CALL GSPLCI(1)
      CALL GSTXCI(1)
      CALL SET(0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
      CALL PERIM(1,1,1,1)
      Y1=0.0
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
      LABLS='ABR'
      CALL MYFRAME(NFRAME,NPLT,0.085,LABLS)
      RETURN
      END
