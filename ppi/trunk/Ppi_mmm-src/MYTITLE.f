c
c----------------------------------------------------------------------X
c
      SUBROUTINE MYTITLE(LABSTDIN,INDAT,NFRAME,BGFLAG)
C
C  ROUTINE TO ADD TITLE FRAME(S) - 31 LINES PER FRAME
C     FROM 0.95 TO 0.05, SPACED 0.03 UNTIL 'END'
C  Always plots copyright frame and standard input.
C  Can also plot color tables (PLOT=.TRUE.).
C
      INCLUDE 'colors.inc'
      CHARACTER*8 INDAT(10),JNDAT(10)
      CHARACTER*60 LABSTDIN(12)
      CHARACTER LAB*64,LABL*34,LABLS*3,BGFLAG*1,TEXT*80

C     SET BACKGROUND (0) AND FOREGROUND (1) COLORS
C        WHITE IS ALL 1'S; BLACK IS ALL 0'S
C
      IF(INDAT(2).EQ.'WHITE   ')THEN
         BGFLAG='W'
      ELSE
         BGFLAG=' '
      END IF
      CALL SFLUSH
      IF(BGFLAG.EQ.'W')THEN
         CALL GSCR(1,0,1.,1.,1.)
         CALL GSCR(1,1,0.,0.,0.)
      ELSE
         CALL GSCR(1,0,0.,0.,0.)
         CALL GSCR(1,1,1.,1.,1.)
      END IF
      CALL GSPLCI(1)
      CALL GSTXCI(1)
      WRITE(6, 5)
 5    FORMAT(1X,'TITLE:')

C     Initialize and plot new TITLE frame
C
 6    CONTINUE
      CALL SET(0.02,0.98,0.02,0.98,0.02,0.98,0.02,0.98,1)
      CALL PERIM(1,1,1,1)
      Y1=0.98      
      
 18   Y1=Y1-0.03
      IF(Y1 .GE. 0.05)THEN
 20      READ(5,21)(JNDAT(I),I=1,10)
 21      FORMAT(10A8)
c         WRITE(6,22)(JNDAT(I),I=1,10)
c 22      FORMAT('Kardin=',10A8)
         IF(JNDAT(1)(1:1).EQ.'*')GO TO 20
         IF(JNDAT(1).EQ.'END     ')THEN
            LABLS='ALL'
            CALL MYFRAME(NFRAME,NPLT,0.085,LABLS)
            WRITE(6,23)
 23         FORMAT(1X,'   END')
            RETURN
         END IF
         IF(JNDAT(1).NE.'        ')THEN
            WRITE(6,25)
 25         FORMAT(1X,'*** MYTITLE: NO END LINE ENCOUNTERED ***')
            STOP
         END IF
         LAB=JNDAT(2)//JNDAT(3)//JNDAT(4)//JNDAT(5)//
     +        JNDAT(6)//JNDAT(7)//JNDAT(8)//JNDAT(9)
         WRITE(6,27)LAB
 27      FORMAT(8X,A64)
         CALL PLCHMQ(.04,Y1,LAB,14.0,0.0,-1.0)
         GO TO 18
      ELSE
         LABLS='ALL'
         CALL MYFRAME(NFRAME,NPLT,0.085,LABLS)
         GO TO 6
      END IF

C     Add frames of input (stdin) to ppi program 
C
      ENTRY MYSTDIN(LABSTDIN,INDAT,NFRAME,BGFLAG)

      IF(INDAT(2).EQ.'WHITE   ')THEN
         BGFLAG='W'
      ELSE
         BGFLAG=' '
      END IF
      CALL SFLUSH
      IF(BGFLAG.EQ.'W')THEN
         CALL GSCR(1,0,1.,1.,1.)
         CALL GSCR(1,1,0.,0.,0.)
      ELSE
         CALL GSCR(1,0,0.,0.,0.)
         CALL GSCR(1,1,1.,1.,1.)
      END IF
      CALL GSPLCI(1)
      CALL GSTXCI(1)

C     Initialize and plot copyright frame
C
      Y1=0.90
      CALL PERIM(1,1,1,1)
      LABL='  *** NCAR/MMM PPI PROGRAM ***  '
      print *,'********* BEGIN PROGRAM STANDARD INPUT *********'
      CALL PLCHMQ(0.5,Y1,LABL,20.0,0.0,0.0)
      Y1=Y1-0.2
      DO I=1,12
        Y1=Y1-0.02
         CALL PLCHMQ(0.5,Y1,LABSTDIN(I),14.0,0.0,0.0)
      END DO
      LABLS='ALL'
      CALL MYFRAME(NFRAME,NPLT,0.085,LABLS)

C     Initialize and start plot of new STDIN frame
C
      IF(INDAT(2).EQ.'WHITE   ')THEN
         BGFLAG='W'
      ELSE
         BGFLAG=' '
      END IF
      CALL SFLUSH
      IF(BGFLAG.EQ.'W')THEN
         CALL GSCR(1,0,1.,1.,1.)
         CALL GSCR(1,1,0.,0.,0.)
      ELSE
         CALL GSCR(1,0,0.,0.,0.)
         CALL GSCR(1,1,1.,1.,1.)
      END IF
      CALL GSPLCI(1)
      CALL GSTXCI(1)

 32   Y1=0.965
      CALL PERIM(1,1,1,1)
      IF(NFRAME.EQ.1)THEN
         LABL='  *** PROGRAM STANDARD INPUT ***  '
      ELSE
         LABL='*** STANDARD INPUT (CONTINUED) ***'
      END IF
      CALL PLCHMQ(0.5,Y1,LABL,12.0,0.0,0.0)
      Y1=Y1-0.005

 34   Y1=Y1-0.02
      IF(Y1 .GE. 0.04)THEN
         READ(5,35,END=40)TEXT
 35      FORMAT(A80)
         WRITE(6,37)TEXT
c 37      FORMAT('Kardin=',A80)
 37      FORMAT(A80)
         CALL PLCHMQ(.04,Y1,TEXT,12.0,0.0,-1.0)
         GO TO 34
      ELSE
         LABLS='ALL'
         CALL MYFRAME(NFRAME,NPLT,0.085,LABLS)
         GO TO 32
      END IF

 40   REWIND 5
      LABLS='ALL'
      CALL MYFRAME(NFRAME,NPLT,0.085,LABLS)
      print *,'********** END PROGRAM STANDARD INPUT **********'
      print *,' '

      RETURN
      END
