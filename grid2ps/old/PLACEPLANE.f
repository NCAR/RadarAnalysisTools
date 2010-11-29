      SUBROUTINE PLACEPLANE(RBUF,LF,KOT,FLDNAM,NPLANE)
C
C
C
      DIMENSION RBUF(126,126)
      INTEGER I,J
      CHARACTER*2 FLDNAM(4)

      IF(KOT .EQ. 3 .AND. LF .EQ. 8) THEN
      PRINT*,FLDNAM
      PRINT*,KOT
      OPEN(16,FILE='/dt/hardt/ced/ced.fill',
     X       ACCESS = 'SEQUENTIAL',
     X       FORM = 'FORMATTED',STATUS = 'new',IOSTAT=ICODE)
        IF(ICODE .GT. 0) THEN
          PRINT *,"UNABLE TO OPEN OUTPUT FILE"
          STOP       
        ENDIF
       WRITE (16,900)((RBUF(I,J),I=1,126),J=1,126)
      ENDIF
      IF(KOT .EQ. 3 .AND. LF .EQ. 7) THEN
      PRINT*,FLDNAM
      PRINT*,KOT
      OPEN(17,FILE='/dt/hardt/ced/ced.contour',
     X       ACCESS = 'SEQUENTIAL',
     X       FORM = 'FORMATTED',STATUS = 'new',IOSTAT=ICODE)
        IF(ICODE .GT. 0) THEN
          PRINT *,"UNABLE TO OPEN OUTPUT FILE"
          STOP       
        ENDIF
       WRITE (17,900)((RBUF(I,J),I=1,126),J=1,126)
 900  FORMAT(10F8.1)
      END IF
      END
