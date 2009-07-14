c
c----------------------------------------------------------------------X
c
      SUBROUTINE GETNLD(INDAT,TNLD,XNLD,YNLD,HNLD,AZNLD,PNLD,MXNL,
     X     INLD,OLAT,OLON,ANGXAX,CNLD,DTNLD,ORLAT,ORLON,DZNLD)
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
      CHARACTER NLDFILE*32,IDIR*4
      CHARACTER*41 HEADER
      CHARACTER*8 INDAT(10)
      CHARACTER*4 BLANK
      LOGICAL FIRST
      INTEGER POS_CG,POS_PERCENT,NEG_CG,NEG_PERCENT

      DATA INLD_MOD/20/
      DATA BLANK/'    '/
      DATA TORAD,TODEG/0.017453293,57.29577951/

      IF(INDAT(9)(1:4).EQ.BLANK)THEN
         READ(INDAT,3)IDIR,OLAT,OLON,CNLD,DTNLD,DZNLD
 3       FORMAT(/////A4,4X,/F8.0/F8.0/4X,F4.0/F4.0,F4.0)
      ELSE
         READ(INDAT,4)IDIR,OLAT,OLON,ANGXAX,CNLD,DTNLD,DZNLD
 4       FORMAT(/////A4,4X,/F8.0/F8.0/F4.0,F4.0/F4.0,F4.0)
      END IF
C
C     SPOL      39.76114   -102.09347 1103.0
C     CHIL      39.234605  -102.27791 1285.0
C     KGLD      39.36694   -101.70027 1122.8
C
      IF(OLAT.EQ.0.0 .AND. ORLAT.NE.0.0)OLAT=ORLAT
      IF(OLON.EQ.0.0 .AND. ORLON.NE.0.0)OLON=ORLON
      IF(IDIR.EQ.'WEST')THEN
         OLON=+1.0*ABS(OLON)
      ELSE
         OLON=-1.0*ABS(OLON)
      END IF
      NLDFILE=INDAT(2)//INDAT(3)//INDAT(4)//INDAT(5)
      WRITE(6,5)NLDFILE,IDIR,OLAT,OLON,ANGXAX
 5    FORMAT(1X,'GETNLD: CG LIGHTNING POSITION FILE= ',A32,A4,4X,
     X     2F12.5,F8.1)
      OPEN(UNIT=9,FILE=NLDFILE,STATUS='OLD')
      FIRST = .TRUE.
      
C     Loop over reading of NLDN data file
C
      INLD=0
      POS_CG=0
      NEG_CG=0

 10   READ(9,11,ERR=20,END=20)HEADER
 11   FORMAT(A41)
      IF(HEADER(1:2).EQ.'//')THEN
         WRITE(6,11)HEADER
      ELSE IF (HEADER(1:2).EQ.'00')THEN
         GO TO 10
      ELSE
         READ(HEADER,13)IMON,IDAY,IHR,IMN,ISC,PLAT,PLON,POLAR
 13      FORMAT(I2,1X,I2,4X,I2,1X,I2,1X,I2,F7.0,F9.0,F8.0)
         IF(FIRST)THEN
            IDAYOLD=IDAY
            FIRST = .FALSE.
         END IF
      END IF
      IF(IMON .EQ. 0 .AND. IDAY .EQ. 0 .AND.
     X   PLAT .EQ. 90.0 .AND. PLON .EQ. -190.0 .AND. 
     X   POLAR .EQ. -1000.0)GO TO 10
      IF(IDAY.NE.IDAYOLD)THEN
         IHR=IHR+24
      END IF

      INLD=INLD+1
      IF(INLD.GT.MXNL)THEN
         INLD=INLD-1
         GO TO 20
      END IF

      IF(IDIR.EQ.'WEST')THEN
         PLON=+1.0*ABS(PLON)
      ELSE
         PLON=-1.0*ABS(PLON)
      END IF
      CALL LL2XYDRV(PLAT,PLON,X,Y,OLAT,OLON,ANGXAX)
      XNLD(INLD)=X
      YNLD(INLD)=Y
      TNLD(INLD)=IHR*3600+IMN*60+ISC
      PNLD(INLD)=POLAR
      HRNG=SQRT(X*X+Y*Y)
      IF(POLAR.GT.0.0)THEN
         POS_CG=POS_CG+1
      ELSE
         NEG_CG=NEG_CG+1
      END IF

      IF(Y.EQ.0.0.AND.X.EQ.0.0)THEN
         AZ=180.0
      ELSE IF(Y.EQ.0.0.AND.X.GT.0.0)THEN
         AZ=90.0
      ELSE IF(Y.EQ.0.0.AND.X.LT.0.0)THEN
         AZ=270.0
      ELSE
         AZ=TODEG*ATAN2(X,Y)
      END IF
      IF(AZ.LT.0.0)THEN
         AZIM=AZ+360.0
      ELSE
         AZIM=AZ
      END IF
      HNLD(INLD)=HRNG
      AZNLD(INLD)=AZIM

      IF(INLD.EQ.1 .OR. MOD(INLD,INLD_MOD).EQ.0)THEN
         WRITE(6,15)INLD,IHR,IMN,ISC,PLAT,PLON,POLAR,
     +        X,Y,PNLD(INLD),HRNG,AZIM
 15      FORMAT(1X,'I,HH:MM:SS,LL,P=',I8,1X,I2.2,':',I2.2,':',I2.2,
     +        2F10.4,F8.1,'  X,Y,P=',3F8.2,' HRNG,AZ=',2F8.2)
      END IF
      GO TO 10

 20   CONTINUE
      CLOSE (UNIT=9)

      IF(MOD(INLD,10).NE.0)THEN
         WRITE(6,15)INLD,IHR,IMN,ISC,PLAT,PLON,POLAR,
     +        X,Y,PNLD(INLD),HRNG,AZIM
      END IF
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



