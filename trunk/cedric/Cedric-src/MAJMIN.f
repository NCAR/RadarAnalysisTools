      SUBROUTINE MAJMIN  (POSX,DELRGX,IFMTX,MAJORX,MINORX,NDIGX,ISZX,
     X                    POSY,DELRGY,IFMTY,MAJORY,MINORY,NDIGY,ISZY)
C                                             R. VAUGHAN      MARCH 1977
C                             MODIFICATION:  L. JAY MILLER    March 1999
C   INPUT:
C       DELRG  IS THE ABSOLUTE DIFFERENCE IN KM FOR WHICH MAJOR AND MINOR
C            DIVISIONS ARE TO BE CALCULATED.
C            DELRG  IS UNCHANGED IN THIS SUB PROGRAM.
C
C   OUTPUT:
C       IFMT, FORMAT OF THE VALUES AT MAJOR DIVISIONS.
C       MAJOR, MAJOR DIVISIONS OF AXIS
C       MINOR, MINOR DIVISIONS OF AXIS
C
C
      PARAMETER (MAXT=51)
      DIMENSION FLIM(MAXT)
      CHARACTER*8 CTEMP,IFMTX,IFMTY
C
      DATA FLIM / 1000., 900.,800.,700.,600.,500.,400.,300.,250., 200.,
     X             100.,  90., 80., 70., 60., 50., 40., 30., 25.,  20.,
     X             10. ,  9. , 8. , 7. , 6. , 5. , 4. , 3. , 2.5,  2. ,
     X             1.  , .9  ,.8  ,.7  ,.6  ,.5  ,.4  ,.3  ,.25 , .2  ,
     X            .1   ,.09 ,.08 ,.07 ,.06 ,.05 ,.04 ,.03  ,.025,.02  ,
     X           .01/
      DATA MININT,MAXINT/5,20/
      DATA EPS/0.001/
C
c      print *,'Majmin: posx,delrgx,posy,delrgy=',posx,delrgx,posy,delrgy

      DO 5 I=1,MAXT
         MAJORX=DELRGX/FLIM(I)+EPS
         IF(ABS((FLIM(I)*MAJORX)-DELRGX).LE.EPS.AND.MAJORX.GE.MININT)
     X      GO TO 10
    5 CONTINUE
      MAJORX=1
      GO TO 20
   10 CONTINUE
      IF(MAJORX.GT.MAXINT) GO TO 20
C
C        GOOD VALUE FOR MAJOR
C
      IF(MAJORX.LE.10) MINORX=10
      IF(MAJORX.GT.10.AND.MAJORX.LE.15) MINORX=5
      IF(MAJORX.GT.15) MINORX=2
      GO TO 30
   20 CONTINUE
C
C        NOT SO GOOD VALUE OF MAJOR
C
      MINORX=MAJORX
      MAJORX=1
   30 CONTINUE

      DO 25 I=1,MAXT
         MAJORY=DELRGY/FLIM(I)+EPS
         IF(ABS((FLIM(I)*MAJORY)-DELRGY).LE.EPS.AND.MAJORY.GE.MININT)
     X      GO TO 210
 25   CONTINUE
      MAJORY=1
      GO TO 220
 210  CONTINUE
      IF(MAJORY.GT.MAXINT) GO TO 220
C
C        GOOD VALUE FOR MAJOR
C
      IF(MAJORY.LE.10) MINORY=10
      IF(MAJORY.GT.10.AND.MAJORY.LE.15) MINORY=5
      IF(MAJORY.GT.15) MINORY=2
      GO TO 230
 220  CONTINUE
C
C        NOT SO GOOD VALUE OF MAJOR
C
      MINORY=MAJORY
      MAJORY=1
 230  CONTINUE


      IF (((DELRGX/MAJORX)-INT(DELRGX/MAJORX)).LE.EPS .AND.
     X     ABS(POSX-INT(POSX)).LE.EPS .AND.
     X    ((DELRGY/MAJORY)-INT(DELRGY/MAJORY)).LE.EPS .AND.
     X     ABS(POSY-INT(POSY)).LE.EPS) THEN
C
C     LABELS ARE INTS
C
         IF (POSX.LT.0.0) THEN
            POSTMP=ABS(POSX)*10.
         ELSE
            POSTMP=POSX
         END IF
         NDIGX=1.001+ALOG10(AMAX1(ABS(POSTMP),ABS(POSX+DELRGX)))
         WRITE(CTEMP,400)NDIGX
 400     FORMAT('(I',I1,')')
         IF (NDIGX.GE.4) THEN
            ISZX=1
         ELSE
            ISZX=2
         END IF
      ELSE
C
C     LABELS ARE FLOATS
C
         NDIGX=6
         ISZX=1
         IDEC=0
         IF(DELRGX.LE.20.) IDEC=1
         WRITE (CTEMP,300)IDEC
 300     FORMAT('(F6.',I1,')')
      END IF
      READ (CTEMP,500)IFMTX
 500  FORMAT(A6)

      IF (((DELRGX/MAJORX)-INT(DELRGX/MAJORX)).LE.EPS .AND.
     X     ABS(POSX-INT(POSX)).LE.EPS .AND.
     X    ((DELRGY/MAJORY)-INT(DELRGY/MAJORY)).LE.EPS .AND.
     X     ABS(POSY-INT(POSY)).LE.EPS) THEN
C
C     LABELS ARE INTS
C
         IF (POSY.LT.0.0) THEN
            POSTMP=ABS(POSY)*10.
         ELSE
            POSTMP=POSY
         END IF
         NDIGY=1.001+ALOG10(AMAX1(ABS(POSTMP),ABS(POSY+DELRGY)))
         WRITE(CTEMP,400)NDIGY
         IF (NDIGY.GE.4) THEN
            ISZY=1
         ELSE
            ISZY=2
         END IF
      ELSE
C
C     LABELS ARE FLOATS
C
         NDIGY=6
         ISZY=1
         IDEC=0
         IF(DELRGY.LE.20.) IDEC=1
         WRITE (CTEMP,300)IDEC
      END IF
      READ (CTEMP,500)IFMTY

      IF (ISZX.NE.ISZY) THEN
         ISZX=1
         ISZY=1
      END IF

      RETURN
      END
