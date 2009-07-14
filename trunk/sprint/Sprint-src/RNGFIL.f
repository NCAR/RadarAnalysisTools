      SUBROUTINE RNGFIL(KRD)
C
C        FILLS COMMON /CRNG/
C        FORMAT OF RES CARD
C
C
C   VARIABLE   DESCRIPTION                  FIELD   NOTES
C   --------   -----------                  -----   -----
C
C   KOMM       'RES'                         P1     COMMAND
C   ELTUS      FIXED ANGLE TOLERANCE         P2
C   MNBEM      MINIMUM NUMBER OF BEAMS       P3
C   RUSR1      STARTING RANGE                P4     IN KM
C   RUSR2      ENDING RANGE                  P5     IN KM
C   RNOTM      BEGINNING GATE                P6     CORRECTION IN M
C   DRGM       GATE SPACING                  P7     IN M
C   CFAC       RADAR CONSTANT                P8
C   VNYQ       NYQUIST VELOCITY              P9
C
C     ELTUS   - Acceptable departure of actual "fixed" angle from the nominal
C               fixed angle.  Beams with |Fixed - Actual| > ELTUS are tossed,
C               where Actual = (PPI) elevation angle, or (RHI) azimuth angle.
C     FXSTOL  - When using the fixed angle (ISWUSR = 'F'IXED) to delineate 
C               scans, all beams within FXSTOL of the first beam in the 
C               sequence belong within the same sweep regardless of sweep 
C               number.  When using the sweep number (ISWUSR = 'N'UMBER) 
C               to delineate scans, beams with the same sweep number are 
C               processed together and their angular positions are assigned 
C               to the nominal fixed angle, regardless of their actual position.
C               FXSTOL is specified with the PROCESS command (see PROFIL) and 
C               is passed around with the /CPRO/ common block.
C   Note: RNOTUS and DRGUS are converted to km from user-supplied values 
C         RNOTM  and DRGM in m.
C
C
      CHARACTER*8 KRD(10)
      CHARACTER*8 CTEMP1
      CHARACTER MSGDEF*14,MSGRA1*14,MSGRA2*14,MSGGAT*14,
     X          MSGSPC*14,MSGRAD*14,MSGNYQ*14
      DATA MSGDEF/'UNALTERED     '/
      COMMON /CRNG/ELTUS,MNBEM,RUSR1,RUSR2,RG1,RG2,DRG,NRG,VNYQ,
     X     RNOTUS,DRGUS,CFAC1,CFAC2,CFAC3
      COMMON /CRNGC/ IRCFXL
      CHARACTER*8 IRCFXL

      CHARACTER*2 IELT
      CHARACTER*8 IBL,KRNOT,KDRG,KCFAC,KVNYQ
      DATA IBL/' '/
      READ (KRD,101)ELTUS,TMPMIN,RUSR1,RUSR2,RNOTM,DRGM,
     X                   FAC,VNYQ,IRCFXL
 101  FORMAT(/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/F8.0/A8)
C  101 FORMAT(8X,8F8.0,A8)
C
      IF (KRD(8).NE.' ') CFAC1=FAC
      MNBEM = TMPMIN
C
      RNOTUS=RNOTM*0.001
      DRGUS= DRGM*0.001
      IF(ELTUS.LE.0.0) ELTUS=360.0
      MNBEM=MAX0(MNBEM,2)
      PRINT 885
885   FORMAT(//5X,'SUMMARY OF RESET COMMAND ')
      PRINT 887
887   FORMAT(5X,'------- -- ----- ------- ')
C
      MSGRA1 = MSGDEF
      MSGRA2 = MSGDEF
      MSGGAT = MSGDEF
      MSGSPC = MSGDEF
      MSGRAD = MSGDEF
      MSGNYQ = MSGDEF
C
      IF (RUSR1.NE.0.0.OR.RUSR2.NE.0.0) THEN
         WRITE (MSGRA1,111)RUSR1
111      FORMAT(F7.2,' KM    ')
         WRITE (MSGRA2,112)RUSR2
112      FORMAT(F7.2,' KM    ')
      END IF
C
      READ (KRD,313)KRNOT,KDRG,KCFAC,KVNYQ
 313  FORMAT(/////A8/A8/A8/A8)
C313   FORMAT(40X,4A8)
C
      IF (KRNOT.NE.' ') THEN
         WRITE (MSGGAT,113)RNOTM
113      FORMAT(F7.0,' METERS')
      END IF
C
      IF (KDRG.NE.' ') THEN
         WRITE (MSGSPC,114)DRGM
114      FORMAT(F7.0,' METERS')
      END IF
C
      IF (KCFAC.NE.' ') THEN
         WRITE (MSGRAD,115)CFAC1
115      FORMAT(F7.2,'       ')
         CFAC2=CFAC1
         CFAC3=CFAC1
      END IF

C
      IF (KVNYQ.NE.' ') THEN
         WRITE (MSGNYQ,116)VNYQ
116      FORMAT(F7.2,' M/SEC ')
      END IF
C
      PRINT 888, MSGRA1,MSGRA2,ELTUS,MNBEM,MSGGAT,MSGSPC,MSGRAD,MSGNYQ
888   FORMAT(/8X,'                  STARTING RANGE: ',A14
     X       /8X,'                    ENDING RANGE: ',A14
     X       /8X,'ANGLE TOLERANCE (|ACTUAL-FIXED|): ',F7.2,' DEGREES',
     X       /8X,'MINIMUM NUMBER OF BEAMS PER SCAN: ',I5
     X       /8X,'CORRECTION TO FIRST GATE IN FILE: ',A14
     X       /8X,'     USER-SPECIFIED GATE SPACING: ',A14
     X       /8X,'   USER-SPECIFIED RADAR CONSTANT: ',A14
     X       /8X,' USER-SPECIFIED NYQUIST VELOCITY: ',A14)
C
C        ESTABLISH FIXED ANGLE MODE
C
      WRITE (CTEMP1,500)IRCFXL
 500  FORMAT(A8)
      READ (CTEMP1,117)IELT,ELBIGN
  117 FORMAT(A2,1X,F5.0)
      I=AMAX1(ELBIGN,0.0)
      IF(IELT.EQ.'MD') THEN
         PRINT 889, I
  889    FORMAT(8X,'FIXED ANGLE OF EACH SCAN WILL BE ',
     X          'RECOMPUTED USING THE MOST FREQUENTLY'/8X,
     X          '   OCCURRING ANGLE OF ALL THE BEAMS IN THE SCAN',
     X      /8X,'   EXCLUDING THE FIRST AND LAST',I5,'.'//)
      ELSE IF(IELT.EQ.'MN') THEN
         PRINT 890, I
  890    FORMAT(8X,'FIXED ANGLE OF EACH SCAN WILL BE ',
     X          'RECOMPUTED USING THE'/8X,'   MEAN ANGLE OF',
     X          ' ALL BEAMS IN THE SCAN'/8X,'   EXCLUDING THE',
     X          ' FIRST AND LAST',I5,'.'//)
      ELSE
         IRCFXL=IBL
         PRINT 891
  891   FORMAT(8X,'RECORDED VALUE OF FIXED ANGLE WILL BE USED.'//)
      END IF
      IF (RUSR1.GE.0.0 .AND. RUSR2.GE.0.0 .AND.
     X            RUSR2.GE.RUSR1) RETURN
      CALL TPQERX(327,1)
      RETURN
      END
