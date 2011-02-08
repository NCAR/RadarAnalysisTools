      program split_nld
C
C     Split the NLD data files into +CG  (7) and -CG (8)
C
      CHARACTER*8 INDAT(10)

C     Common blocks for NLDN positional information
C
      PARAMETER (MXNL=100000)
      COMMON/NLDN/XNLD(MXNL),YNLD(MXNL),TNLD(MXNL),PNLD(MXNL),
     X            HNLD(MXNL),AZNLD(MXNL),INLD,DTNLD

      OPEN(UNIT=5,FILE='./split_nld.inp',STATUS='OLD')
    5 READ(5, 7)(INDAT(I),I=1,10)
    7 FORMAT(10A8)

      IF(INDAT(1)(1:1).EQ.'*')THEN
         GO TO 5
      ELSE IF(INDAT(1).EQ.'GETNLD  ')THEN
         GO TO 500
      ELSE IF(INDAT(1).EQ.'STOP')THEN
         STOP
      END IF

 500  CALL GETNLD(INDAT,TNLD,XNLD,YNLD,HNLD,AZNLD,PNLD,MXNL,INLD,
     X     OLAT,OLON,ANGXAX,DTNLD,ORLAT,ORLON,DZNLD)
      GO TO 5

      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE GETNLD(INDAT,TNLD,XNLD,YNLD,HNLD,AZNLD,PNLD,MXNL,
     X     INLD,OLAT,OLON,ANGXAX,DTNLD,ORLAT,ORLON,DZNLD)
C
C  READ IN NLDN CG STRIKE POSITIONS (TIME-LAT-LON-POLARITY) FROM NLDFILE:
C     This routine assumes that two successive days have been combined 
C     into a single file to handle crossing midnight (UTC).  Twenty-four
C     will be added to the hour of the second day.
C
C     CONVERT THEM TO (X,Y) FROM (OLAT,OLON)
C  NOTE: (OLAT,OLON) MUST BE THE (LAT,LON) OF THE PLOTTING WINDOW ORIGIN,
C        WHERE THE WINDOW IS DEFINED BY (GXMIN,GXMAX,GYMIN,GYMAX)
C
C  EXAMPLE FOR NATIONAL:
C
C  // File D00138.nldn opened on Wed May 17 16:54:16 2000
C  05/17/:0 16:53:40 39.300  -86.038   -80.0  1
C
C     TNLD      - Time of the strike (sec)
C     XNLD,YNLD - (X,Y) position (km) of CG strike relative to origin
C     HNLD      - Horizontal range (km) of CG strike
C     AZNLD     - Azimuth angle (deg) of CG strike
C     PNLD      - Polarity of ground strike and value (Kv/m)
C     DTNLD     - Time interval for plotting strikes.  Plot points within
C                 a time window relative to the current radar scan.
C                 DTNLD .gt. 0: Plot +/- DTNLD from central time (RMSEC)
C                 DTNLD .le. 0: Plot within DTNLD seconds outside the
C                               radar scan time interval
C     DZNLD     - Height/Azimuth interval for plotting CG strike points
C
      DIMENSION XNLD(MXNL),YNLD(MXNL),TNLD(MXNL),PNLD(MXNL)
      DIMENSION HNLD(MXNL),AZNLD(MXNL)
      CHARACTER NLDFILE*32,POSFILE*10,NEGFILE*10
      CHARACTER*71 HEADER
      CHARACTER*8 INDAT(10)
      INTEGER POS_CG,POS_PERCENT,NEG_CG,NEG_PERCENT

      DATA INLD_MOD/20/

      NLDFILE=INDAT(2)//INDAT(3)//INDAT(4)//INDAT(5)
      WRITE(6,5)NLDFILE
 5    FORMAT(1X,'GETNLD: CG LIGHTNING POSITION FILE= ',A32)
      WRITE(POSFILE,7)NLDFILE(1:7)
 7    FORMAT(A7,'pos')
      WRITE(NEGFILE,8)NLDFILE(1:7)
 8    FORMAT(A7,'neg')
      print *,'NLDFILE=',NLDFILE      
      print *,'POSFILE=',POSFILE       
      print *,'NEGFILE=',NEGFILE

      OPEN(UNIT=9,FILE=NLDFILE,STATUS='OLD')
      OPEN(UNIT=10,FILE=POSFILE,ACCESS='SEQUENTIAL',STATUS='NEW')
      OPEN(UNIT=11,FILE=NEGFILE,ACCESS='SEQUENTIAL',STATUS='NEW')
      
C     Loop over reading of NLDN data file
C
      INLD=0
      POS_CG=0
      NEG_CG=0

 10   READ(9,11,ERR=20,END=20)HEADER
 11   FORMAT(A71)
      IF(HEADER(1:2).EQ.'//')THEN
         WRITE(6,11)HEADER
         POLAR=0.0
      ELSE
         READ(HEADER,13)IMON,IDAY,IHR,IMN,ISC,PLAT,PLON,POLAR
 13      FORMAT(I2,1X,I2,4X,I2,1X,I2,1X,I2,F7.0,F9.0,F8.0)
         INLD=INLD+1
      END IF

      WRITE(6,15)INLD,IHR,IMN,ISC,PLAT,PLON,POLAR
 15   FORMAT(1X,'I,HH:MM:SS,LL,P=',I8,1X,I2.2,':',I2.2,':',I2.2,
     +     2F10.4,F8.1)
      IF(POLAR.EQ.-1000.0)GO TO 10

      IF(POLAR.EQ.0.0)THEN
         WRITE(10,11)HEADER
         WRITE(11,11)HEADER
      ELSE IF(POLAR.GT.0.0)THEN
         WRITE(10,11)HEADER
         POS_CG=POS_CG+1
      ELSE IF (POLAR.LT.0.0)THEN
         WRITE(11,11)HEADER
         NEG_CG=NEG_CG+1
      END IF
      GO TO 10

 20   CONTINUE
      CLOSE (UNIT=9)
      CLOSE (UNIT=10)
      CLOSE (UNIT=11)

      POS_PERCENT=NINT(100.0*POS_CG/INLD)
      NEG_PERCENT=NINT(100.0*NEG_CG/INLD)
      WRITE(6,25)INLD
 25   FORMAT(1X,'   Total CG strikes = ',I8)
      WRITE(6,27)POS_CG,POS_PERCENT
 27   FORMAT(1X,'Positive CG strikes = ',I8,' or ',I8,' percent')
      WRITE(6,29)NEG_CG,100*NEG_CG/INLD
 29   FORMAT(1X,'Negative CG strikes = ',I8,' or ',I8,' percent')

      RETURN
      END



