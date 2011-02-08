c
c----------------------------------------------------------------------X
c
      SUBROUTINE SAVSWTH(INDAT,SWTHDAT,NSWTH,NSWPAVG,MXPLT,WHICH_AHST,
     X     WHICH_ASCT)
C
C  STORE SWATH CONTOUR PLOT CHARARACTERISTICS FOR LATER EXECUTION
C     INDAT   - INPUT ARRAY CONTAINING CHARACTERISTICS FOR ONE PLOT
C     SWTHDAT - OUTPUT ARRAY CONTAINING ALL (MXPLT) PLOT CHARACTERISTICS
C     NSWTH   - NUMBER OF PLOTS TO BE DONE
C     NSWPAVG - NUMBER OF SWEEPS TO SWATH.  IF NSWPAVG .LE. 0, THEN ALWAYS
C               DO SWATH PLOTS AT END-OF-VOLUME OR END-OF-TAPE.
C
C     Flags for accumulated plots (histograms and scattergrams):
C
C        WHICH_AHST - (SWP) accumulated histograms after each sweep
C                           default if can't recognize WHICH_AHST
C                     (EOV) accumulated histograms only after EOV
C                     (NON) turn off accumulated histograms
C        WHICH_ASCT - (SWP) accumulated scattergrams after each sweep
C                           default if can't recognize WHICH_ASCT
C                     (EOV) accumulated scattergrams only after EOV
C                     (NON) turn off accumulated scattergrams
C
      CHARACTER*8 INDAT(10),SWTHDAT(10,MXPLT)
      CHARACTER*3 WHICH_AHST,WHICH_ASCT
      CHARACTER*3 BLANK
      DATA BLANK/'   '/

      NSWTH=NSWTH+1
      IF(NSWTH.GT.MXPLT)THEN
         WRITE(6,11)MXPLT
 11      FORMAT(1X,'*** MAXIMUM NUMBER OF PLOTS (',I3,') EXCEEDED ***')
         STOP
      END IF

      IF(INDAT(1).EQ.'CNTSWTH ')THEN
         IF(INDAT(4).EQ.'RESET   ')THEN
            READ(INDAT(6),13)RSWPAVG
 13         FORMAT(F8.0)
            NSWPAVG=NINT(RSWPAVG)
            IF(NSWPAVG.LE.0 .OR. NSWPAVG.GE.9999)NSWPAVG=999999
         ELSE
            NSWPAVG=999999
         END IF
      END IF

      IF(INDAT(1).EQ.'PLTAHST ')THEN
         IF(INDAT(2).EQ.'CLRBINS')THEN
            READ(INDAT(3),15)RSWPAVG
 15         FORMAT(F8.0)
            NSWPAVG=NINT(RSWPAVG)
            IF(NSWPAVG.LE.0 .OR. NSWPAVG.GE.9999)NSWPAVG=999999
         ELSE
            NSWPAVG=999999
         END IF
         WHICH_AHST = 'NON'
         READ(INDAT(4)(1:3),16)WHICH_AHST
 16      FORMAT(A3)
         IF(WHICH_AHST .EQ. BLANK)WHICH_AHST='SWP'
         IF(WHICH_AHST .NE. 'SWP' .AND. 
     X      WHICH_AHST .NE. 'EOV')THEN
            WHICH_AHST = 'SWP'
         END IF
      END IF

      IF(INDAT(1).EQ.'PLTASCT ')THEN
         IF(INDAT(2).EQ.'CLRSCTS')THEN
            READ(INDAT(3),17)RSWPAVG
 17         FORMAT(F8.0)
            NSWPAVG=NINT(RSWPAVG)
            IF(NSWPAVG.LE.0 .OR. NSWPAVG.GE.9999)NSWPAVG=999999
         ELSE
            NSWPAVG=999999
         END IF
         WHICH_ASCT = 'NON'
         READ(INDAT(4)(1:3),18)WHICH_ASCT
 18      FORMAT(A3)
         IF(WHICH_ASCT .EQ. BLANK)WHICH_ASCT='SWP'
         IF(WHICH_ASCT .NE. 'SWP' .AND. 
     X      WHICH_ASCT .NE. 'EOV')THEN
            WHICH_ASCT = 'SWP'
         END IF
      END IF

      PRINT 55,(INDAT(I),I=1,10),NSWTH
 55   FORMAT(1X,' SWTH: ',10A8,' NSWTH=',I8)
      DO 70 I=1,10
         SWTHDAT(I,NSWTH)=INDAT(I)
 70   CONTINUE

      print *,' SWTH: nswpavg,which_ahst/asct=',
     +     nswpavg,which_ahst,'+',which_asct,'+'

      RETURN
      END
