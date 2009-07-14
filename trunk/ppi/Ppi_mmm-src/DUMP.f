c
c----------------------------------------------------------------------X
c
      SUBROUTINE DUMP(INDAT,IFD,NDUMP,NRST)
C
C  SET FLAG (IFD) TO DUMP (NDUMP) BEAMS OF DATA
C     HDUMP - Controls beam housekeeping dumping
C     BDUMP - Controls dumping of fields
C
      CHARACTER*8 INDAT(10)

      WRITE(6,9)(INDAT(I),I=1,10)
    9 FORMAT(1X,10A8)
      READ(INDAT,11)HDUMP,BDUMP
   11 FORMAT(/F8.0/F8.0)
      IF(HDUMP.GT.0.0 .OR. BDUMP.GT. 0.0)THEN
         IFD=1
      ELSE
         IFD=0
      END IF
      NRST=NINT(HDUMP)
      NDUMP=NINT(BDUMP)
      IF(NDUMP.GT.0 .AND. NRST.EQ.0)NRST=999
      WRITE(6,13)NRST,NDUMP
   13 FORMAT(10X,' PRINT EVERY',I3,'-TH BEAM OF HOUSEKEEPING',/,
     +       10X,' DUMP',I5,' BEAMS OF FIELD VALUES')
      RETURN
      END
