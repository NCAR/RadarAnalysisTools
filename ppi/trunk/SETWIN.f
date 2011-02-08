c
c----------------------------------------------------------------------X
c
      SUBROUTINE SETWIN(INDAT,NROW,NCOL,NWIN,XRT,YTP,SIDEX,SIDEY,
     X           IWIN,LABX,LABY,ILFLG,XBTP,YBTP,SIZBX)

C  SET THE NUMBER OF PLOTS PER FRAME
C
C     NROW        - NUMBER OF ROWS OF PLOTS
C     NCOL        - NUMBER OF COLUMNS OF PLOTS
C     NWIN        - NUMBER OF PLOTS PER FRAME.  NWIN DEFAULTS TO NROW*NCOL
C     XRT,YTP     - TOP RIGHT COORIDNATES OF PLOT WINDOW FOR THE ITH PLOT.
C     SIDEX,SIDEY - SIZE OF PLOT WINDOW IN THE X AND Y DIRECTIONS
C     IWIN        - POINTER TO THE CURRENT PLOT WINDOW
C     LABX,LABY   - AXIS LABELING FLAGS FOR THE X AND Y AXIS
C     ILFLG       - COLOR BAR LABELING FLAG:
C                   0 - PUT COLOR BAR FOR EACH PLOT;
C                   1 - PUT COLOR BAR ONLY ONCE
C     XBTP,YBTP   - TOP X AND Y COORDINATES FOR THE COLOR BAR
C                   LOCATION FOR THE ITH PLOT
C     SIZBX       - SIZE OF THE COLOR BAR IN THE X DIRECTION
C
      DIMENSION XRT(26),YTP(26),SIDEX(26),SIDEY(26),LABX(26),
     +          LABY(26),XBTP(26),YBTP(26)
      DATA X1,Y2,SIZE/0.08,0.9,0.82/
      DATA FRACT/0.90/
      DATA NROLD,NCOLD,NWOLD/0,0,0/
      CHARACTER*8 INDAT(10)

      READ(INDAT,20)ROW,COL,WIN,FLG
 20   FORMAT(/F8.0/F8.0/F8.0/F8.0)
      NROW=NINT(ROW)
      NCOL=NINT(COL)
      NWIN=NINT(WIN)
      ILFLG=NINT(FLG)

C     NROW .LE. 0 resets SETWIN back to single panel
C     NWIN .LE. 1 uses default SETWIN for a single panel
C        Note: Uses XRT(1),YTP(1),SIDEX(1),SIDEY(1) that
C              are set in data statement in PPI_MMM.
C
      IF(NROW.LE.0 .OR. NWIN.LE.1)THEN
         NROW=1
         NCOL=1
         NWIN=1
         IWIN=1
         LABX(1)=1
         LABY(1)=1
         NROLD=NROW
         NCOLD=NCOL
         NWOLD=NWIN
         RETURN
      END IF

C     RETURN IF MULTIPLE PLOTS PER FRAME REMAINS UNCHANGED
C     OTHERWISE, RESET WINDOW POINTER (IWIN) AND REDO
C
      IF(NROW.EQ.NROLD.AND.NCOL.EQ.NCOLD.AND.NWIN.EQ.NWOLD)RETURN
      IWIN=2

C     Entry point without SETWIN command to finish during-volume,
C     multiple-plot frames which are only partially filled before 
C     continuing with end-of-volume plots.  See PPI_MMM.
C
      ENTRY SETWIN2(INDAT,NROW,NCOL,NWIN,XRT,YTP,SIDEX,SIDEY,
     X     IWIN,LABX,LABY,ILFLG,XBTP,YBTP,SIZBX)
c      write(*,*)'setwin: ',iwin-1,nrow,ncol,nwin
      NROLD=NROW
      NCOLD=NCOL
      NWOLD=NWIN
      SIZE=0.82-(NCOL-1)*0.07
      ILFLG=NINT(FLG)
      IF(NWIN.LE.0)NWIN=NROW*NCOL
      IF(NWIN.GT.25)THEN
         WRITE(6,23)
 23      FORMAT('***PLOTS/FRAME EXCEEDS MAX ALLOWED (25)***')
         STOP
      END IF
      K=2

C     DETERMINE THE LOCATION, SIZE AND LABELING FLAGS OF THE ITH PLOT
C
      IF(NCOL.EQ.1)THEN
         XLFT=0.5-0.5*SIZE
      ELSE
         XLFT=X1
      END IF
      DO 50 I=1,NROW
         DO 50 J=1,NCOL
            SIDEX(K)=SIZE/NCOL
            SIDEY(K)=SIZE/NROW
            LABX(K)=0
            LABY(K)=0
            IF(I.EQ.NROW)LABX(K)=1
            IF(I*J.GE.NWIN)LABX(K)=1
            IF(J.EQ.1)LABY(K)=1
            YTP(K)=Y2-(I-1)*SIDEY(K)
            XRT(K)=XLFT+SIDEX(K)*J
            SIDEX(K)=FRACT*SIDEX(K)
            SIDEY(K)=FRACT*SIDEY(K)
 50   K=K+1
      K=2

C     DETERMINE THE LOCATION AND SIZE OF THE COLOR BARS
C     ILFLG - COLOR BAR FLAG
C             (1) ONLY ONE , (0) ONE FOR EACH PLOT
C
      IF(ILFLG.EQ.1)THEN
         SIZBX=1.0-XLFT-SIZE
         DO 60 I=1,NROW
            DO 60 J=1,NCOL
               XBTP(K)=XLFT+SIZE
                YBTP(K)=Y2
 60      K=K+1
      ELSE
         SIZBX=(1.0-(XLFT+SIZE))/NCOL
         DO 70 I=1,NROW
            DO 70 J=1,NCOL
               XBTP(K)=(J-1)*SIZBX+XLFT+SIZE
               YBTP(K)=YTP(K)
 70      K=K+1
      END IF
      RETURN
      END



