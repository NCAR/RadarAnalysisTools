      SUBROUTINE CEDQUIT(BEGSEC)
      INTEGER BEGSEC
Cf2py intent(in) BEGSEC
      character*8 tyme
      integer endsec,totsec,totmin,remsec
C     
C     TERMINATE THE SESSION
C     
      CALL CLOCK(TYME)
      READ (TYME,11)IHR,IMN,ISC
 11   format(i2,1x,i2,1x,i2)
      ENDSEC = ISC + 60*IMN + 3600*IHR
      TOTSEC = ENDSEC - BEGSEC
      TOTMIN = INT(FLOAT(TOTSEC)/60.0)
      REMSEC= TOTSEC - 60*TOTMIN
      PRINT *,' '
      WRITE(LPR,903)
 903  FORMAT(72('+'))
      WRITE(LPR,905)TYME,TOTSEC,TOTMIN,REMSEC
 905  FORMAT('CEDRIC EXECUTION ENDED AT ',A8, 
     X     ' TAKING ',I6,' SECONDS OR MM:SS=',I3.2,':',I2.2)
      WRITE(LPR,903)

      CALL CEDERX(500,1)
      RETURN
      END
