c
c----------------------------------------------------------------------X
c
      SUBROUTINE PLT_GRIDS(INDAT,PLT_NMRK,MXK,KMRK,NXGRID,NYGRID,
     X     DEL_GRID,NEXGRIDS)
C
C  READ SPECIFICATIONS FOR SEVERAL GRIDS SURROUNDING
C  SPECIFIC LANDMARK NAME TO BE PLOTTED 
C     PLT_NMRK - Specific landmark names to be plotted
C
      CHARACTER*7 PLT_NMRK(MXK)
      CHARACTER*8 INDAT(10)

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
C        PLTGRIDS - NEXRAD grids are plotted if .TRUE.
C
      PARAMETER (MXKK=1000)
      LOGICAL PLTCLOCK,PLTPERCNT,PLTGRIDS
      INTEGER TIME_BEG,TIME_END,REQ_RADS,REC_RADS,REC_HOURS,REQ_HOURS
      CHARACTER*11 DATE_BEG,DATE_END
      CHARACTER*14 NEX_NAME(MXKK)
      COMMON/NEXCLOCK/PLTCLOCK,PLTPERCNT,PLTGRIDS,TIME_BEG,TIME_END,
     X     REQ_RADS,REC_RADS,REQ_HOURS,REC_HOURS,TPERCENT,DATE_BEG,
     X     DATE_END,NEX_NAME

      DIMENSION NXGRID(NEXGRIDS),NYGRID(NEXGRIDS),DEL_GRID(NEXGRIDS)

      I=0
      KMRK=1
      PLT_NMRK(1)=INDAT(2)(1:7)

 12   READ(5,13)(INDAT(I),I=1,10)
 13   FORMAT(10A8)
      IF(INDAT(1)(1:3).EQ.'END')THEN
         PLTGRIDS=.TRUE.
         RETURN
      ELSE
         I=I+1
         READ(INDAT,15)RNX,RNY,DEL_GRID(I)
 15      FORMAT(/F8.0/F8.0/F8.0)
         NXGRID(I)=INT(RNX)
         NYGRID(I)=INT(RNY)
         GO TO 12
      END IF

      RETURN
      END
