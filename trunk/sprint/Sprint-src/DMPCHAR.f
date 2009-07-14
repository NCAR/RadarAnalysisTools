c
c----------------------------------------------------------------------X
c
      SUBROUTINE DMPCHAR(ICH,IRECL)
C
C     PRINT A CHARACTER ARRAY OF IRECL WORDS
C
      CHARACTER*8 ICH(IRECL)
      DO 40 I=1,IRECL,10
         J=I+9
         IF(J.GT.IRECL) J=IRECL
         WRITE(7,34) I, (ICH(L), L=I,J)
   34    FORMAT(1X,'CWD ',I4,' =',10(2X,A8))
   40 CONTINUE
      RETURN
      END
