      SUBROUTINE CEDINIT(BEGSEC)
      INTEGER BEGSEC
C
Cf2py intent(out) BEGSEC
C
C     Call the initialization routines required for cedric main.
C
      character*8 tyme
      COMMON LCMB(1),MEMUSE
C     
C     INTIALIZE EDIT FILE CHARACTERISTICS (RAM AND/OR DISK FILE)
C     
      CALL CGETMEM(LCMB,MEMUSE)
C     
C     INITIALIZE THE SYMBOL TABLE- GENERATE LOWER CASE LETTERS
C     TEMPORARY SITUATION BECAUSE OF COMMUMNICATIONS
C     PROTOCOLS.
C     
      CALL SYMINT
      CALL VERSOUT

C     INITIALIZE C VARIABLES STRUCTURE USED FOR WRITING OUT NETCDF
      CALL icstruct()

      call clock(tyme)
      read (tyme,11)ihr,imn,isc
 11   format(i2,1x,i2,1x,i2)
      BEGSEC = isc + 60*imn + 3600*ihr
      print *,' '
      print *,'++++++++++++++++++++++++++++++++++++'
      print *,'Cedric execution started at ',tyme
      print *,'++++++++++++++++++++++++++++++++++++'
      print *,' '

      RETURN
      END
