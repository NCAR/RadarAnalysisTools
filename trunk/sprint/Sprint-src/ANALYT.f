      SUBROUTINE ANALYT(KRD,RFNAM,FNAM,FNUM,P1,P2,P3,P4,P10)
C
C     THIS SUBROUTINE PARSES THE 'ANALYT' CARD FOR INFO
C     ABOUT WHAT FIELD TO REPLACE, AND WHAT FUNCTION TO REPLACE IT WITH.
C
C     FNUM IS ANALYTICAL FUNCTION NUMBER (ANLACT is called during execution).
C     P1,P2,P3,P4 ARE PARAMETERS FOR THE FUNCTIONS
C
C     FNUM FUNCTION   DESCRIPTION             P1    P2    P3    P4
C     ---- --------   -----------            ----  ----  ----  ----
C      01  CON        Constant value         Value
C      02  VR         Radial velocity        U     V     0.0
C      03  VR         Radial velocity        U     V     W
C      04  RANDOM     Uniform random number  Min   Max
C      05  NORMAL     Gaussian random number Mean  Std
C      06  COSPROD    Cosine product         Ampl  x-Wvl y-Wvl z-Wvl
C      07  VRS        Vr satisfy continuity  Ampl  x-Wvl y-Wvl z-Wvl
C      08  RANGE      Slant ranges
C      09  ELEV       Elevation angles
C      10  AZIM       Azimuth angles
C      11  SQUARE     Square-wave product    Ampl  x-Wvl y-Wvl z-Wvl
C                     Airborne only
C      12  ROTANG     Rotation angle
C      13  TILT       Track rel. tilt angle
C      14  RANGEXZ    Range in x-z plane
C      15  YDIST      Y-Distance along track
C
C      00             ANALYT command included, but function asked 
C                     for doesn't exist.
C      P10='ACTUAL' ==> Use actual elevation angle in calculations
C
      CHARACTER*8 KRD(10),RFNAM,FNAM,P10
      INTEGER FNUM
C
C     PARSE THE INPUT LINE
C
      READ (KRD,100)RFNAM,FNAM,P1,P2,P3,P4,P10
 100  FORMAT(/A8/A8/F8.2/F8.2/F8.2/F8.2///A8)

      IF (FNAM.EQ.'CON') THEN
C
C     CONSTANT FIELD
C
         FNUM=1
         WRITE(*,105)
 105     FORMAT(//5X,'SUMMARY OF ANALYT COMMAND')
         WRITE(*,110)
 110     FORMAT(5X,'------- -- ------ -------')
         WRITE(*,120)RFNAM
 120     FORMAT(5X,'REPLACE FIELD:',A8)
         WRITE(*,140)P1
 140     FORMAT(5X,'WITH CONSTANT:',F8.2,/)
      ELSE IF (FNAM.EQ.'VR') THEN
C
C     CALCULATE RADIAL VELOCITY
C
         IF (P3.EQ.0.0) THEN
            FNUM=2
            WRITE(*,105)
            WRITE(*,110)
            WRITE(*,145)RFNAM
 145        FORMAT(5X,'REPLACE ',A8,' WITH RADIAL VELOCITY')
            WRITE(*,170)P1,P2
 170        FORMAT(5X,'U=',F8.2,' V=',F8.2,' W=0.0',/)
         ELSE
            FNUM=3
            WRITE(*,105)
            WRITE(*,110)
            WRITE(*,150)RFNAM
 150        FORMAT(5X,'REPLACE ',A8,' WITH RADIAL VELOCITY')
            WRITE(*,175)P1,P2,P3
 175        FORMAT(5X,'U=',F8.2,' V=',F8.2,' W=',F8.2,/)
         END IF
      ELSE IF (FNAM.EQ.'RANDOM') THEN
C
C     UNIFORM RANDOM FIELD 
C
         FNUM=4
         WRITE(*,105)
         WRITE(*,110)
         WRITE(*,160)RFNAM,P1,P2
 160     FORMAT(5X,'REPLACE ',A8,' WITH UNIFORMLY DISTRIBUTED RANDOM',
     X         ' VARIABLE BETWEEN ',F8.2, ' AND ',F8.2,/)
      ELSE IF (FNAM.EQ.'NORMAL') THEN
         FNUM=5
         WRITE(*,105)
         WRITE(*,110)
         WRITE(*,180)RFNAM,P1,P2
 180     FORMAT(5X,'REPLACE ',A8,' WITH NORMALLY DISTRIBUTED RANDOM',
     X          ' VARIABLE WITH MEAN ',F8.2,' AND SIGMA ',F8.2,/)
      ELSE IF (FNAM.EQ.'COSPROD') THEN
C
C     PRODUCT OF 2 COSINES
C
         FNUM=6
         WRITE(*,105)
         WRITE(*,110)
         WRITE(*,190)RFNAM
 190     FORMAT(5X,'REPLACE ',A8,' WITH P1*COS(2*PI*X/P2)*',
     X          'COS(2*PI*Y/P3)*COS(2*PI*Z/P4)',/)
         WRITE(*,195)P1,P2,P3,P4
 195     FORMAT(5X,'WHERE  P1=',F8.2,' P2=',F8.2,' P3=',F8.2,
     X          ' P4=',F8.2, 'AND PI=3.14159265',/)
      ELSE IF (FNAM.EQ.'VRS') THEN
C
C     SPECIAL RADIAL VELOCITY THAT SATISFIES THE CONTINUITY EQUATION
C
         FNUM=7
         WRITE(*,105)
         WRITE(*,110)
         WRITE(*,200)RFNAM,P1,P2,P3,P4
 200     FORMAT(5X,'REPLACE ',A8,' WITH A RADIAL VELOCITY WHERE',/
     X          7X,'U=((P1)*P2/(4*PI))*SIN(2*PI*X/P2)*',
     X          'COS(2*PI*Y/P3)*COS(2*PI*Z/P4)'/,
     X          7X,'V=((P1)*P3/(4*PI))*COS(2*PI*X/P2)*',
     X          'SIN(2*PI*Y/P3)*COS(2*Z*Z/P4)'/,
     X          7X,'W=((-P1)*P4/(2*PI))*COS(2*PI*X/P2)*',
     X          'COS(2*PI*Y/P3)*SIN(2*PI*Z/P4)',/,
     X          7X,'AND WHERE P1=',F8.2,'  P2=',F8.2,
     X          '  P3=',F8.2,'  P4=',F8.2,'  AND PI=3.14159265',/)

      ELSE IF (FNAM.EQ.'RANGE') THEN
C
C     REPLACE FIELD WITH RANGE VALUES OF POINTS
C
         FNUM=8
         WRITE(*,230)RFNAM
 230     FORMAT(5X,'REPLACE ',A8,' WITH RANGE VALUES ')
      ELSE IF (FNAM.EQ.'ELEV') THEN
C
C     REPLACE FIELD WITH ACTUAL ELEVATION OF POINTS
C
         FNUM=9
         WRITE(*,250)RFNAM
 250     FORMAT(5X,'REPLACE ',A8,' WITH ELEVATION VALUES ')
         
      ELSE IF (FNAM.EQ.'AZIM') THEN
C
C     REPLACE FIELD WITH HORIZONTAL AZIMUTH VALUES
C
         FNUM=10
         WRITE(*,270)RFNAM
 270     FORMAT(5X,'REPLACE ',A8,' WITH HORIZONTAL AZIMUTH VALUES ')
      ELSE IF (FNAM.EQ.'SQUARE') THEN
C
C     REPLACE FIELD WITH PRODUCT OF 3 SQUARE WAVES
C
         FNUM=11
         WRITE(*,290)RFNAM
 290     FORMAT(5X,'REPLACE ',A8,' WITH P1*SIGN(COS(2*PI*X/P2))*',
     X          'SIGN(COS(2*PI*Y/P3))*SIGN(COS(2*PI*Z/P4))',/)
         WRITE(*,195)P1,P2,P3,P4
      ELSE IF (FNAM.EQ.'ROTANG') THEN
C     REPLACE FIELD WITH ROTATION ANGLE (AIRBORNE ONLY)
         FNUM=12
         WRITE(*,292)RFNAM
 292     FORMAT(5X,'REPLACE ',A8,' WITH ROTATION ANGLE',/)
      ELSE IF (FNAM.EQ.'TILT') THEN
         FNUM=13
         WRITE(*,294)RFNAM
 294     FORMAT(5X,'REPLACE ',A8,' WITH TRACK RELATIVE TILT ANGLE',/)
      ELSE IF (FNAM.EQ.'RANGEXZ') THEN
         FNUM=14
         WRITE(*,296)RFNAM
 296     FORMAT(5X,'REPLACE ',A8,' WITH RANGE IN X-Z PLANE',/)
      ELSE IF (FNAM.EQ.'YDIST') THEN
         FNUM=15
         WRITE(*,298)RFNAM
 298     FORMAT(5X,'REPLACE ',A8,' WITH Y-DIST (DIST. ALONG TRACK)',/)
      ELSE
         FNUM=0
      END IF

      IF (P10.EQ.' ') P10='ACTUAL'
      WRITE(*,500)P10
 500  FORMAT(5X,'ELEVATION ANGLE USED FOR CALCULATIONS WILL BE:',A8)
      RETURN
      END
         
      
