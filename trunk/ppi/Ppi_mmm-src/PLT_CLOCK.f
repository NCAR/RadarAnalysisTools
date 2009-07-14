c
c----------------------------------------------------------------------X
c
      SUBROUTINE PLT_CLOCK(INDAT,PLT_NMRK,PLT_TIME,REC_DATA,MXK,MXT,
     X     KMRK,HCIRC,NEX_DATE,HOUR_BEG,HOUR_END,NHOURS)
C
C  READ FROM NEXRAD INVENTORY FILE SPECIFIC LANDMARK NAMES TO BE PLOTTED
C  ANNOTATES WITH 24-hour CLOCK.
C     NEX_DATE - Date of requested NEXRAD data 
C     HOUR_BEG - Beginning hour of data
C     HOUR_END - Ending hour of data
C     NHOURS   - Number of hours covered by all radars 
C     PLT_NMRK - Specific landmark names to be plotted
C     PLT_TIME - Number of volume scans realized for each hour
C                of a 72 hour period for each NEXRAD ordered
C     REC_DATA - (Y) Some data was received, 
C                (N) No data was received, but it was ordered
C     REC_HOUR - Number of hours of coverage by this radar or '-no'
C     HCIRC - Radius of 24-hr circle
C
C     Information in COMMON/NEXCLOCK/ passed to LABEL:
C
C        PLTCLOCK - NEXRAD data clock is plotted if .TRUE.
C        TIME_BEG - Beginning hour of NEXRAD data
C        TIME_END - Ending    hour of NEXRAD data
C        REQ_RADS - Number of NEXRADs requested
C        REC_RADS - Number of NEXRADs received
C        REC_HOURS- Total number of hours received
C        REQ_HOURS- Total number of hours requested
C        TPERCENT - Total percent requested/received
C        DATE_BEG - Beginning date of NEXRAD data
C        DATE_END - Ending    date of NEXRAD data
C        NEX_NAME - NEXRAD radar name for plot label
C        PLTPERCNT- NEXRAD data percent is plotted if .TRUE.
C
      PARAMETER (MXKK=1000)
      LOGICAL PLTCLOCK,PLTPERCNT
      INTEGER TIME_BEG,TIME_END,REQ_RADS,REC_RADS,REC_HOURS,REQ_HOURS
      CHARACTER*11 DATE_BEG,DATE_END
      CHARACTER*14 NEX_NAME(MXKK)
      COMMON/NEXCLOCK/PLTCLOCK,PLTPERCNT,TIME_BEG,TIME_END,
     X     REQ_RADS,REC_RADS,REQ_HOURS,REC_HOURS,TPERCENT,
     X     DATE_BEG,DATE_END,NEX_NAME

      CHARACTER*7 PLT_NMRK(MXK)
      CHARACTER*1 PLT_TIME(MXK,MXT),REC_DATA(MXK)
      CHARACTER*3 REC_HOUR(MXKK)
      CHARACTER*8 INDAT(10),BLANK
      CHARACTER*32 CLKFILE
      CHARACTER*84 LINE
      CHARACTER*11 NEX_DATE
      CHARACTER*4 HOUR(73)
      INTEGER HR_START,HOUR_BEG,HOUR_END

      DATA BLANK/'        '/

      READ(INDAT,11)NEX_DATE(1:8),NEX_DATE(9:11),HCIRC
 11   FORMAT(/////A8/A3,5X/F8.0)
      CLKFILE=INDAT(2)//INDAT(3)//INDAT(4)//INDAT(5)
      WRITE(6,13)CLKFILE,NEX_DATE,HCIRC
 13   FORMAT(/,1X,'PLT_CLOCK: INVENTORY FILE= ',A32,A11,5X,F8.0)
      OPEN(UNIT=9,FILE=CLKFILE,STATUS='OLD')

      KMRK=0
 20   CONTINUE
      READ(9,21,END=990)LINE
      WRITE(6,21)LINE
 21   FORMAT(A83)

      IF(LINE(1:11).EQ.BLANK .AND. LINE(12:15).NE.'    ')THEN

C     Extract potential starting hour from the line
C
         READ(LINE(12:13),23)HR_START
 23      FORMAT(I2)
         GO TO 20
      END IF
      IF(LINE(1:11).NE.NEX_DATE)GO TO 20

C     For the requested date:
C        1) Assign the beginning hour
C        2) Read lines containing radar names and
C        3) Find the last ending hour of all times
C
      HOUR_BEG=HR_START
      NHOURS=0
      REC_RADS=0
 30   READ(9,21,END=20)LINE
      WRITE(6,21)LINE
      IF(LINE(8:8).EQ.'K')THEN
         ICNT=0
         DO I=13,84
            IF(LINE(I:I).NE.' '.AND.LINE(I:I).NE.'.')THEN
               ICNT=ICNT+1
            END IF
         END DO
         KMRK=KMRK+1
         PLT_NMRK(KMRK)=LINE(8:11)
         IF(ICNT.GT.0)THEN
            REC_DATA(KMRK)='Y'
            WRITE(REC_HOUR(KMRK),31)ICNT
 31         FORMAT('-',I2.2)
            REC_RADS=REC_RADS+1
         ELSE
            REC_DATA(KMRK)='N'
            WRITE(REC_HOUR(KMRK),33)
 33         FORMAT('-no')
         END IF
         DO N=1,72
            NN=N+12
            M=MOD(N-1,24)
            IF(LINE(NN:NN).NE.' ')THEN
               NHOURS=MAX(N,NHOURS)
               PLT_TIME(KMRK,N)=LINE(NN:NN)
            ELSE 
               IF (M.EQ.0)THEN
                  PLT_TIME(KMRK,N)='d'
               ELSE
                  PLT_TIME(KMRK,N)='x'
               END IF
            END IF
         END DO
         GO TO 30
      ELSE
         GO TO 40
      END IF

 40   CONTINUE
      HOUR_END = HOUR_BEG + (NHOURS - 1)
      print *,'Plt_clock: date,hour_beg.end,nhrs,kmrk=',
     +     nex_date,hour_beg,hour_end,nhours,kmrk
      DO N=1,73
         IHOUR=100*MOD(HOUR_BEG+N-1,24)
         WRITE(HOUR(N),41)IHOUR
 41      FORMAT(I4.4)
      END DO
      WRITE(*,43)(HOUR(N),N=1,73,6)
 43   FORMAT(11X,73(A4,2X))
      WRITE(*,45)NEX_DATE
 45   FORMAT(A11,1X,3('|     |     |     |     '),'|')
      DO K=1,KMRK
c         WRITE(NEX_NAME(K),46)PLT_NMRK(K)(1:4),REC_DATA(K)
c 46      FORMAT(3X,A4,'-',A1)
         WRITE(NEX_NAME(K),46)PLT_NMRK(K)(1:4),REC_HOUR(K)
 46      FORMAT(2X,A4,A3)
         WRITE(*,47)K,PLT_NMRK(K),(PLT_TIME(K,M),M=1,72),
     X        REC_DATA(K),NEX_NAME(K)
 47      FORMAT(I6,1X,A4,1X,72A1,'-',A1,1X,A9)
      END DO

      PLTCLOCK=.TRUE.
      REQ_RADS=KMRK
      REQ_HOURS=NHOURS
      DATE_BEG=NEX_DATE
      TIME_BEG=100*MOD(HOUR_BEG,24)
      TIME_END=100*MOD(HOUR_END,24)
      IDAYS=NHOURS/24
      READ(DATE_BEG(1:2),51)IDATE
 51   FORMAT(I2)
      IDATE=IDATE+IDAYS
      WRITE(DATE_END,53)IDATE,DATE_BEG(3:11)
 53   FORMAT(I2,A9)
      print *,'Plt_clock: ',date_beg,time_beg,' ',date_end,time_end
      print *,'           ',req_rads,req_hours,rec_rads
      RETURN

 990  WRITE(*,991)
 991  FORMAT('+++ END-OF-DATA: DATE REQUESTED WAS NOT FOUND +++')
      STOP
      END
