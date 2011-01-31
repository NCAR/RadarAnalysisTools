c
c----------------------------------------------------------------------X
c
      SUBROUTINE DMPINTGR(IB,IRECL)
C
C     PRINT AN INTEGER ARRAY OF IRECL WORDS
C
      DIMENSION IB(IRECL)
      DO 40 I=1,IRECL,15
         J=I+14
         IF(J.GT.IRECL) J=IRECL
         WRITE(7,34) I, (IB(L), L=I,J)
   34    FORMAT(1X,'IWD ',I4,' =',15I8)
   40 CONTINUE
      RETURN
      END
