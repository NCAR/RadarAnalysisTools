      SUBROUTINE SAVEARRAY(RBUF,NPLANE)
C
C
C
      DIMENSION RBUF(256,256)
      INTEGER I,J,XDIM,YDIM
      CHARACTER*30 FILENAM(30)

      PRINT*
      PRINT*, 'Number of points in x: '
      READ*, XDIM
      PRINT*
      PRINT*, 'Number of points in y: '
      READ*, YDIM
      PRINT*
      PRINT*, 'Output filename: '
      READ*, FILENAM(30)
      OPEN(16,FILE=FILENAM(30),
     X       ACCESS = 'SEQUENTIAL',
     X       FORM = 'FORMATTED',STATUS = 'new',IOSTAT=ICODE)
        IF(ICODE .GT. 0) THEN
          PRINT *,"UNABLE TO OPEN OUTPUT FILE"
          STOP       
        ENDIF
       WRITE (16,900)((RBUF(I,J),I=1,XDIM),J=1,YDIM)
 900  FORMAT(10F8.1)
      END
