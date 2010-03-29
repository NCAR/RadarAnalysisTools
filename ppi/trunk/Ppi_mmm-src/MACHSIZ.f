c
c----------------------------------------------------------------------X
c
      SUBROUTINE MACHSIZ(INDAT,DEC,DECWR,WORDSZ)
C
C  SET MACHINE AND WORD SIZE, DETERMINE IF SWAPPING IS REQUIRED.
C     DEC   - (1) Reading input on DEC,     (0) Reading input on non-DEC
C     DECWR - (1) Input was written on DEC, (0) Input was written on non-DEC
C
C                 If (DEC=1 and DECWR=0) or (DEC=0 and DECWR=1), 
C                 byte-swapping is required.
C
C     WORDSZ - 32 (most workstations) or 64 (Cray/YMP) bits
C
C              DECRD   = 'DECRD',   DEC=1, else DEC=0
C              DECWRT  = 'DECWRT' , DECWR=1, else DECWR=0
C
      CHARACTER*8 INDAT(10),DECRD,DECWRT

      WRITE(6,9)(INDAT(I),I=1,10)
 9    FORMAT(1X,10A8)
      READ(INDAT,10)DECRD,DECWRT,WORDSZ
 10   FORMAT(/A8/A8/F8.0)

      IF(WORDSZ.NE.32.0 .AND. WORDSZ.NE.64.0)WORDSZ=32.0
      WRITE(6,11)INT(WORDSZ)
 11   FORMAT(8X,'WORD SIZE = ',I2,' BITS')

      IF(DECRD.EQ.'DECRD')THEN
         DEC=1.0
         WRITE(6,13)
 13      FORMAT(8X,'Reading input on DEC machine')
      ELSE
         DEC=0.0
         WRITE(6,15)
 15      FORMAT(8X,'Reading input on non-DEC machine')
      END IF

      IF(DECWRT.EQ.'DECWRT')THEN
         DECWR=1.0
         WRITE(6,17)
 17      FORMAT(8X,'Input data was written on DEC machine')
      ELSE
         DECWR=0.0
         WRITE(6,19)
 19      FORMAT(8X,'Input data was written on non-DEC machine')
      END IF

C     BYTE-SWAPPING CHECK
C
      IF((DEC .EQ. 1.0 .AND. DECWR .EQ. 0.0).OR.
     +   (DEC .EQ. 0.0 .AND. DECWR .EQ. 1.0))THEN
         WRITE(6,21)
 21      FORMAT(8X,'   *** BYTE-SWAPPING REQUIRED ***',/)
      END IF

      RETURN
      END






