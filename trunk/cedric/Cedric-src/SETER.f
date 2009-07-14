      SUBROUTINE SETER(MESSG,NERR,IOPT)
C
C  SETER SETS LERROR = NERR, OPTIONALLY PRINTS THE MESSAGE AND DUMPS
C  ACCORDING TO THE FOLLOWING RULES...
C
C    IF IOPT = 1 AND RECOVERING      - JUST REMEMBER THE ERROR.
C    IF IOPT = 1 AND NOT RECOVERING  - PRINT AND STOP.
C    IF IOPT = 2                     - PRINT, DUMP AND STOP.
C
C  INPUT
C
C    MESSG  - THE ERROR MESSAGE (130 CHARACTERS MAXIMUM)
C    NERR   - THE ERROR NUMBER. MUST HAVE NERR NON-ZERO.
C    IOPT   - THE OPTION. MUST HAVE IOPT=1 OR 2.
C
C  ERROR STATES -
C
C    1 - MESSAGE LENGTH NOT POSITIVE.
C    2 - CANNOT HAVE NERR=0.
C    3 - AN UNRECOVERED ERROR FOLLOWED BY ANOTHER ERROR.
C    4 - BAD VALUE FOR IOPT.
C
C
C  FORCE LOAD OF BLOCKDATA
C
      EXTERNAL UERRBD
      CHARACTER*(*) MESSG
      COMMON /UERRF/IERF
      COMMON /FRAMES/ IFRAME
      DATA ICOUNT /0/

C
C     ADDITION HERE TO MAKE MOST ERRORS RECOVERABLE
C
      IF (IOPT.EQ.1 .AND. NERR.GE.0) THEN
         ICOUNT=ICOUNT+1
         WRITE(*,23)MESSG
 23      FORMAT(A)
         WRITE(*,25)IFRAME
 25      FORMAT(' FRAME NUMBER ',I3,' MAY BE INVALID')
         IF (ICOUNT.GE.5) THEN
            WRITE(*,27)
 27         FORMAT(/,5X,'+++TOO MANY GRAPHICS ERRORS; EXITING+++')
            CALL FLUSH_STDOUT
         END IF
         RETURN
      END IF


C
C  THE UNIT FOR ERROR MESSAGES IS I1MACH(4)
C
      IF (IERF .EQ. 0) THEN
      IERF = I1MACH(4)
      ENDIF
C
      NMESSG = LEN(MESSG)
      IF (NMESSG.GE.1) GO TO 10
C
C  A MESSAGE OF NON-POSITIVE LENGTH IS FATAL.
C
        WRITE(IERF,9000)
 9000   FORMAT(' ERROR    1 IN SETER - MESSAGE LENGTH NOT POSITIVE.')
        GO TO 60
C
   10 CONTINUE
      IF (NERR.NE.0) GO TO 20
C
C  CANNOT TURN THE ERROR STATE OFF USING SETER.
C
        WRITE(IERF,9001)
 9001   FORMAT(' ERROR    2 IN SETER - CANNOT HAVE NERR=0'/
     1         ' THE CURRENT ERROR MESSAGE FOLLOWS'/)
        CALL E9RIN(MESSG,NERR,.TRUE.)
        ITEMP=I8SAV(1,1,.TRUE.)
        GO TO 50
C
C  SET LERROR AND TEST FOR A PREVIOUS UNRECOVERED ERROR.
C
 20   CONTINUE
      IF (I8SAV(1,NERR,.TRUE.).EQ.0) GO TO 30
C
        WRITE(IERF,9002)
 9002   FORMAT(' ERROR    3 IN SETER -',
     1         ' AN UNRECOVERED ERROR FOLLOWED BY ANOTHER ERROR.'//
     2         ' THE PREVIOUS AND CURRENT ERROR MESSAGES FOLLOW.'///)
        CALL EPRIN
        CALL E9RIN(MESSG,NERR,.TRUE.)
        GO TO 50
C
C  SAVE THIS MESSAGE IN CASE IT IS NOT RECOVERED FROM PROPERLY.
C
 30   CALL E9RIN(MESSG,NERR,.TRUE.)
C
      IF (IOPT.EQ.1 .OR. IOPT.EQ.2) GO TO 40
C
C  MUST HAVE IOPT = 1 OR 2.
C
        WRITE(IERF,9003)
 9003   FORMAT(' ERROR    4 IN SETER - BAD VALUE FOR IOPT'//
     1         ' THE CURRENT ERROR MESSAGE FOLLOWS'///)
        GO TO 50
C
C  TEST FOR RECOVERY.
C
 40   CONTINUE
      IF (IOPT.EQ.2) GO TO 50
C
      IF (I8SAV(2,0,.FALSE.).EQ.1) RETURN
C
      CALL EPRIN
      CALL FDUM
      CALL FLUSH_STDOUT
C
 50   CALL EPRIN
 60   CALL FDUM
      CALL FLUSH_STDOUT
C
      END
