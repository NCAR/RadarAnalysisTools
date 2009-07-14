      SUBROUTINE DSPSYN(IUN)
C
C        DISPLAYS CURRENT INFORMATION ASSOCIATED WITH THE INPUT FILES
C                 DESIGNATED FOR THE SYNTHESIS PROCEDURE.
C                 INFORMATION IS SENT TO UNIT IUN.
C
C     MAXTRF - Maximum number of fields that can be transferred within
C              the SYNTHES command (8).
C     MXFS   - Maximum number of fields allowed during synthesis,
C              including those transferred (10).  MXFS = MAXTRF + 2.
C     IRADTB(I) - Actual number of fields that will be transferred for
C                 the Ith radar.
C
      PARAMETER (MXRAD=14,MXFS=10)
      COMMON /ARBRNE/ IABFLG(MXRAD),NFAZEL(2,MXRAD),SCAZEL(2,MXRAD)
      CHARACTER*8 NFAZEL
      COMMON /SYNBLK/ LSYN(MXRAD),NAMRAD(MXRAD),INVOLD(MXRAD),
     X                NFLDIN(MXFS,MXRAD,2),NRQF,IRADTB(MXRAD),NRADS
      CHARACTER*8 NAMRAD,INVOLD,NFLDIN
      CHARACTER*2 IWEQ,ITEST
      DATA IWEQ/'W='/
      CHARACTER*8 CTEMP
      WRITE(IUN,102)
  102 FORMAT(' RADAR NO.',4X,'NAME',5X,'VOLUME DESIGNATION',5X,
     X       'VELOCITY',5X,'WEIGHT')
      NSAV=0
      DO 10 I=1,NRADS
         NSAV=MAX0(IRADTB(I),NSAV)
         M2=1
         WRITE (CTEMP,100)NFLDIN(2,I,1)
 100     FORMAT(A8)
         READ (CTEMP,101)ITEST
  101    FORMAT(A2)
         IF(ITEST.EQ.IWEQ) M2=2
         WRITE(IUN,103) I,NAMRAD(I),INVOLD(I),
     X        NFLDIN(1,I,1),(NFLDIN(2,I,M),M=1,M2)
  103    FORMAT(1X,I5,7X,A6,4X,A8,17X,A8,5X,2A8)
         IF(IABFLG(I).EQ.0) GO TO 10
C
C           AIRBORNE DOPPLER
C
            WRITE(IUN,107) NFAZEL(1,I),NFAZEL(2,I)
  107       FORMAT(23X,'--  AIRBORNE  --       AZ=',A8,'  EL=',A8)
C
   10 CONTINUE
      WRITE(IUN,104)
  104 FORMAT( 9X,' TRANSFERRED FIELDS:')
      IF(NSAV.LE.0) THEN
         WRITE(IUN,105)
  105    FORMAT(12X,' NO ADDITIONAL FIELDS TRANSFERRED.')
      ELSE
         DO 20 I=1,NRADS
            LAST=IRADTB(I)
            IF(LAST.LE.0) GO TO 20
            LAST=LAST+2
            WRITE(IUN,106) I,NAMRAD(I),LAST,(NFLDIN(M,I,1),M=3,LAST)
  106       FORMAT(1X,I5,7X,A6,I5,5X,8A8)
   20    CONTINUE
      END IF
      RETURN
      END
