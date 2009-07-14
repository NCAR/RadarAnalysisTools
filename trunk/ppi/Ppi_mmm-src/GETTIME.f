c	
c----------------------------------------------------------------------X
c
      SUBROUTINE GETTIME(INDAT,IUN,IBTIME,IETIME,IDTIME,ITRANS,IBSWEP,
     X                   IESWEP,TANGMX,ANGINP,IRW,IFMT,DELAMX)
C
C  GET ALL THE CHARACTERISITICS FOR THE PROCESSING LOOP
C
C PROCESS IUN     IBTIME  IETIME  IREW    ITRANS  IDTIME  IBSWEP  IESWEP
C         IUN    - INPUT UNIT NUMBER (CORRESPONDS TO MSREAD FORT.NN)
C         IBTIME - TIME TO BEGIN PROCESSING (DEFAULT=000000)
C         IETIME -   "   "   "       "      (DEFAULT=240000)
C         IREW   - REWIND (Y) OR DON'T REWIND (N) UNIT (IUN) BEFORE
C                  PROCESSING THE SPECIFIED TIME INTERVAL (DEFAULT=YES)
C         ITRANS - PROCESS (Y) OR DON'T PROCESS (N) TRANSITION BEAMS
C                  (DEFAULT=YES)
C         IDTIME - TIME INCREMENT (MIN) FOR SKIPPING DURING PROCESSING.
C                  INCREMENT THE PROCESS BEGIN TIME (IBTIME) BY (IDTIME)
C                  DURING PROCESSING.  (DEFAULT=0.0)
C         IBSWEP - SWEEP NUMBER TO BEGIN PROCESSING (DEFAULT=  1)
C         IESWEP -   "      "    "   "       "      (   "   =999)
C         TANGMX - MAXIMUM WIDTH ALLOWED FOR A SWEEP
C         ANGINP - MINIMUM INCREMENT IN BEAMS TO BE PROCESSED (DEFAULT=0.05)
C         DELAMX - MAXIMUM INCREMENT ALLOWED IN CONTOURING (DEFAULT=5.0)
C
      CHARACTER*8 INDAT(10),IFMT
      CHARACTER*1 ITRANS,IREW

      DATA DF_ANGINP,DF_DELAMX/0.05,5.0/

      READ(INDAT,11)RUN,RBTIME,RETIME,IREW,ITRANS,RDTIME,RBSWEP,RESWEP,
     +     RANGMX,ANGINP,DELAMX
   11 FORMAT(/F8.0/F8.0/F8.0/A1,7X/A1,7X/F8.0/F4.0,F4.0/F8.0/F4.0,F4.0)
      IUN=RUN+.00001
      IF(IUN.LT.10)THEN
         WRITE(6,13)
   13    FORMAT(1X,'*** DO NOT USE ANY UNIT .LT. 10 ***')
         STOP
      END IF
      IF(INDAT(3).EQ.'        ')THEN
         IBTIME=000000
      ELSE
         IBTIME=RBTIME
      END IF
      IF(INDAT(4).EQ.'        ')THEN
         IETIME=240000
      ELSE
         IETIME=RETIME
      END IF
      IF(INDAT(7).EQ.'        ')THEN
         IDTIME=0
      ELSE
         IDTIME=RDTIME
      ENDIF
      IF(INDAT(8)(1:4).EQ.'        ')THEN
         IBSWEP=1
      ELSE
         IBSWEP=RBSWEP
      ENDIF
      IF(INDAT(8)(1:4).EQ.'        ')THEN
         IESWEP=999
      ELSE
         IESWEP=RESWEP
      ENDIF
      IF(INDAT(9).EQ.'        ')THEN
         TANGMX=360.0
      ELSE
         TANGMX=RANGMX
         IF(TANGMX.LE.0.0)TANGMX=360.0
      ENDIF

      WRITE(6,21)IUN,IDTIME,IBTIME,IETIME,IBSWEP,IESWEP
   21 FORMAT(1X,/,'UNIT: ',I3,'  Process every ',I3,' min,',
     +            ' beginning at ',I6.6,' and ending at ',I6.6,
     +            '; include sweeps ',I4,' through ',I4)
      IRW=0
      IF(IREW.EQ.'Y')THEN
          IRW=1
C         REWIND IUN
         WRITE(6,23)IUN
   23    FORMAT(9X,'  Rewinding unit ',I3,
     +             ' before processing is started')
      END IF
      IF(ITRANS.NE.'Y'.AND.ITRANS.NE.'N'.AND.ITRANS.NE.'D')ITRANS='N'
      IF(ITRANS.EQ.'Y')THEN
         WRITE(6,25)
   25    FORMAT(9X,'  Transition beams WILL     be processed')
      ELSE
         WRITE(6,27)
   27    FORMAT(9X,'  Transition beams WILL NOT be processed')
      END IF
      IF(ANGINP.LT.DF_ANGINP)ANGINP=DF_ANGINP
      WRITE(6,29)ANGINP
   29 FORMAT(9X,
     +     '  The minimum beam increment required for processing is ',
     +     F6.2,' degrees')
      IF(DELAMX.LE.0.0)DELAMX=DF_DELAMX
      WRITE(6,31)DELAMX
   31 FORMAT(9X,
     +     '  The maximum beam spacing allowed for contouring is ',
     +     F6.2,' degrees')
      WRITE(6,33)TANGMX
   33 FORMAT(9X,
     +     '  The maximum sweep width allowed is ',F6.2,' deg',/)
      IF(IFMT.NE.'DORADE  ')CALL INIT_COS(IUN,IRW)
      RETURN
      END
