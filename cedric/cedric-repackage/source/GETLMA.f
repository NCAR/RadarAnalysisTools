c     
c----------------------------------------------------------------------X
c
      SUBROUTINE GETLMA(KRD,TLMA,XLMA,YLMA,ZLMA,MXLM,ILMA,OLAT,OLON,
     X     ANGXAX,DTLMA,ORLAT,ORLON,DZLMA)
C
C  READ IN LMA LIGHTNING CHANNEL (TIME-LAT-LON-HEIGHT) FROM LATFILE:
C     CONVERT THEM TO (X,Y,Z) FROM (OLAT,OLON)
C  NOTE: (OLAT,OLON) MUST BE THE (LAT,LON) OF THE PLOTTING WINDOW ORIGIN,
C        WHERE THE WINDOW IS DEFINED BY (GXMIN,GXMAX,GYMIN,GYMAX)
C
C  EXAMPLE FOR NMT:
C
C New Mexico Tech's Lightning Mapping System -- Ananlyzed Data
C Data start time: 07/12/00 23:00:00
C Number of seconds analyzed:  600
C Location: STEPS
C Analysis program: /usr/local/nfs/bin/analysis_v5
C Analysis started : Mon Jan 29 19:51:11 2001
C Analysis finished: Mon Jan 29 19:55:03 2001
C Number of active stations: 11
C Active stations: B C D E F H I J K L N 
C Data: time (UT sec of day), lat, lon, alt(m), reduced chi^2, # of stations contributed
C Data format: 15.9f 10.6f 10.6f 7.1f 5.2f 2d
C Number of events:        10202
C *** data ***
C  82800.002450609  40.175552 -103.544895 10887.1  0.94 11
C  82800.002755029  40.175492 -103.541112 10302.5  0.14  8
C
C     TLMA      - Time of the strike
C     XLMA,YLMA - (X,Y) position of landmark relative to origin
C     ZLMA      - Height of LMA lightning channel
C     DTLMA     - Time interval for plotting strikes.  Plot points within
C                 a time window relative to the current radar scan.
C                 DTLMA .gt. 0: Plot +/- DTLMA from central time (RMSEC)
C                 DTLMA .le. 0: Plot within DTLMA seconds outside the
C                               radar scan time interval
C     DZLMA     - Height interval for plotting strikes.  Plot points within
C                 a height window relative to the current radar scan.
C                 DZLMA .gt. 0: Plot +/- DZLMA from radar scan height
C                 DZLMA .le. 0: Plot within any height interval
C
      DIMENSION XLMA(MXLM),YLMA(MXLM),ZLMA(MXLM)
      DOUBLE PRECISION TLMA(MXLM)
      CHARACTER LATFILE*32,IDIR*4
      CHARACTER*60 HEADER
      CHARACTER*8 KRD(10)
      CHARACTER*8 BLANK
      DOUBLE PRECISION SEC,FSEC,RLAT,RLON

c      DATA ILMA_MOD/1000/
      DATA ILMA_MOD/1/
      DATA BLANK/'        '/
      DATA TORAD,TODEG/0.017453293,57.29577951/

      IF(KRD(9).EQ.BLANK)THEN
         READ(KRD,3)IDIR,OLAT,OLON,DTLMA,DZLMA
 3       FORMAT(/////A4,4X,/F8.0/F8.0//F4.0,F4.0)
      ELSE
         READ(KRD,4)IDIR,OLAT,OLON,ANGXAX,DTLMA,DZLMA
 4       FORMAT(/////A4,4X,/F8.0/F8.0/F8.0/F4.0,F4.0)
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

      LATFILE=KRD(2)//KRD(3)//KRD(4)//KRD(5)
      WRITE(6,5)LATFILE,IDIR,OLAT,OLON,ANGXAX
 5    FORMAT(1X,'GETLMA: IN-CLOUD LIGHTNING POSITION FILE= ',A32,A4,4X,
     X     2F12.5,F8.1)
      OPEN(UNIT=9,FILE=LATFILE,STATUS='OLD')
      
      ILMA=0
 10   READ(9,11,END=20)HEADER
 11   FORMAT(A60)
      IF(HEADER(1:1).NE.' ')THEN
         print *,'SKIP: ',header
         GO TO 10
      END IF
      READ(HEADER,12,ERR=10,END=20)SEC,RLAT,RLON,HGHT,CHISQ,NSTA
 12   FORMAT(F16.9,F11.6,F12.6,F8.1,F6.2,I3)

      IF(CHISQ.GT.20.0)GO TO 10

      ILMA=ILMA+1
      IF(ILMA.GT.MXLM)THEN
         ILMA=ILMA-1
         CLOSE (UNIT=9)
         RETURN
      END IF
      IHR = SEC/3600.0
      IMN = (SEC - IHR*3600.0)/60.0
      ISC = (SEC - IHR*3600.0 - IMN*60.0)
      FSEC= SEC - FLOAT(INT(SEC))

      IF(IDIR.EQ.'WEST')THEN
         PLON=+1.0*ABS(RLON)
      ELSE
         PLON=-1.0*ABS(RLON)
      END IF
      PLAT=RLAT
      CALL LL2XYDRV(PLAT,PLON,X,Y,OLAT,OLON,ANGXAX)
      XLMA(ILMA)=X
      YLMA(ILMA)=Y
      TLMA(ILMA)=SEC
      ZLMA(ILMA)=0.001*HGHT
      Z=0.001*HGHT
      
      IF(ILMA.EQ.1 .OR. MOD(ILMA,ILMA_MOD).EQ.0)THEN
         WRITE(6,15)ILMA,IHR,IMN,ISC,FSEC,PLAT,PLON,HGHT,X,Y,Z,
     X        HRNG,AZIM
 15      FORMAT(1X,'I,HMS,LL,H=',I8,1X,I2.2,':',I2.2,':',I2.2,
     X        F12.9,2F12.6,F8.1,'  X,Y,Z=',3F8.2,' HRNG,AZ=',2F8.2)
      END IF
      GO TO 10

 20   CONTINUE
      CLOSE (UNIT=9)
      RETURN
      END

