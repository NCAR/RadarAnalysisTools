c
c----------------------------------------------------------------------X
c
      SUBROUTINE PLT_PERCENT(INDAT,PLT_NMRK,MXK,KMRK,PERCENT,HCIRC)
C
C  READ FROM NEXRAD INVENTORY FILE PERCENTS FOR THE SEASONS FOR
C  SPECIFIC LANDMARK NAMES TO BE PLOTTED 
C     PERCENT  - Percent of hours requested that were received
C     PLT_NMRK - Specific landmark names to be plotted
C     HCIRC - Radius of 24-hr circle
C
      CHARACTER*7 PLT_NMRK(MXK)
      CHARACTER*3 PERCENT(MXK)
      CHARACTER*8 INDAT(10)
      CHARACTER*32 CNTFILE
      CHARACTER*84 LINE

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

      INTEGER RECEIVED(MXKK),REQUESTED(MXKK)

      READ(INDAT,11)HCIRC
 11   FORMAT(///////F8.0)
      CNTFILE=INDAT(2)//INDAT(3)//INDAT(4)//INDAT(5)
      WRITE(6,13)CNTFILE,HCIRC
 13   FORMAT(/,1X,'PLT_PERCENT: INVENTORY FILE= ',A32,5X,F8.0)
      OPEN(UNIT=9,FILE=CNTFILE,STATUS='OLD')

      KMRK=0
 20   CONTINUE
      READ(9,21,END=40)LINE
      WRITE(6,21)LINE
 21   FORMAT(A83)

      IF(LINE(1:15).EQ.'Hours requested')THEN
         READ(LINE(18:23),23)REQ_HOURS
 23      FORMAT(I4)
         READ(LINE(40:43),23)REC_HOURS
         READ(LINE(47:50),25)TPERCENT
 25      FORMAT(F4.1)
         PRINT *,'PLT_PERCENT: HRS REC/REQ ',REQ_HOURS,REC_HOURS,
     X        ' => ',TPERCENT
      END IF
         
      IF(LINE(18:25).EQ.'requests' .AND. LINE(50:50).EQ.'%')THEN
         
C     Extract NEXRAD name and percent as character strings
C     
         KMRK=KMRK+1
         PLT_NMRK(KMRK)=LINE(1:4)
         PERCENT(KMRK)=LINE(46:48)
         READ(LINE(32:34),31)RECEIVED(KMRK)
         READ(LINE(39:41),31)REQUESTED(KMRK)
 31      FORMAT(I3)
      END IF
      GO TO 20
         
 40   PLTPERCNT=.TRUE.
      REQ_RADS=KMRK
      PRINT *,'PLT_PERCENT: KMRK=',PLTPERCNT,KMRK
      DO K=1,KMRK
c         WRITE(NEX_NAME(K),46)PLT_NMRK(K)(1:4),PERCENT(K)
c 46      FORMAT(2X,A4,A3)
c         PRINT *,'PLT_PERCENT: K,NAME,PERCENT=',K,PLT_NMRK(K),
c     +        PERCENT(K),' ',NEX_NAME(K)
         WRITE(NEX_NAME(K),46)PLT_NMRK(K)(1:4),RECEIVED(K),REQUESTED(K)
 46      FORMAT(A4,'-',I3.3,'/',I3.3)
         PRINT *,'PLT_PERCENT: K,NAME,PERCENT=',K,PLT_NMRK(K),
     +        RECEIVED(K),REQUESTED(K),' ',NEX_NAME(K)
      END DO

      WRITE(*,41)
 41   FORMAT('+++ END-OF-DATA +++')

      RETURN
      END
