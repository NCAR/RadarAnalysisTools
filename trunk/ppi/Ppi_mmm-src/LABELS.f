c
c----------------------------------------------------------------------X
c
      SUBROUTINE LABELS(INDAT,LABLS)
C
C  SET FLAG (LABLS) FOR ABBREVIATED ('ABR') OR ('ALL') LABELS
C
      CHARACTER*8 INDAT(10)
      CHARACTER*3 LABLS

      WRITE(6,9)(INDAT(I),I=1,10)
    9 FORMAT(1X,10A8)
      READ(INDAT,11)LABLS
   11 FORMAT(/A3)
      IF(LABLS.NE.'NON'.AND.
     +   LABLS.NE.'ABR'.AND.
     +   LABLS.NE.'ALL')LABLS='ALL'
      IF(LABLS.EQ.'ABR')THEN
         WRITE(6,13)
   13    FORMAT(10X,' ANNOTATE WITH ABBREVIATED LABELING')
      ELSE IF(LABLS.EQ.'ALL')THEN
         WRITE(6,15)
   15    FORMAT(10X,' ANNOTATE WITH FULL LABELING')
      ELSE IF(LABLS.EQ.'NON')THEN
         WRITE(6,17)
   17    FORMAT(10X,' DO NOT ANNOTATE WITH ANY LABELING')
      END IF
      RETURN
      END
