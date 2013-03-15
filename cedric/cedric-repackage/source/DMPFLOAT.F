c
c----------------------------------------------------------------------X
c
      SUBROUTINE DMPFLOAT(FB,IRECL)
C
C     PRINT A FLOATING POINT ARRAY OF IRECL WORDS
C
      DIMENSION FB(IRECL)
      DO 40 I=1,IRECL,15
         J=I+14
         IF(J.GT.IRECL) J=IRECL
         WRITE(6,34) I, (FB(L), L=I,J)
   34    FORMAT(1X,'FW ',I6,'=',15F8.2)
   40 CONTINUE
      RETURN
      END
