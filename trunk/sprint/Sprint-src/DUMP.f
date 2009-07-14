c
c----------------------------------------------------------------------X
c
      SUBROUTINE DUMP(KRD,IFD,IFD_RAYS)
C
C  SET HOW OFTEN INPUT BEAMS ARE TO BE DUMPED DURING INPUT
C     IFD      - (0) No dumping, (1) Dump one-line beam housekeeping.
C     IFD_RAYS - The skipping interval for beam-dumping.  If non-zero,
C                then dump every IFD_RAYS'th beam.
C
      CHARACTER*8 KRD(10)

      WRITE(6,9)(KRD(I),I=1,10)
 9    FORMAT(1X,10A8)
      READ(KRD,10)RAYS
 10   FORMAT(/F8.0)
      IF(RAYS.NE.0.0)THEN
         IFD=1
         IFD_RAYS=INT(RAYS)
      ELSE
         IFD=0
         IFD_RAYS=1
      END IF

      IF(IFD .NE. 0)THEN
         WRITE(6,11)IFD_RAYS
 11      FORMAT(8X,'DUMP EVERY ',I2,'th BEAM')
         WRITE(6,13)
 13      FORMAT(8X,
     X'+++ WARNING - IMPLEMENTED FOR UF, FF, DORADE, and NEXRAD +++')
      END IF

      RETURN
      END






