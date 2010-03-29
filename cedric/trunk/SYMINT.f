      SUBROUTINE SYMINT
C
C        INITIALIZES THE LOWER CASE LETTERS IN THE SYMBOL TABLE
C
      PARAMETER (NDSYM=27)
      COMMON /SYMTAB/ ISYM(NDSYM,2)
      CHARACTER*1 ISYM
      DATA (ISYM(I,1),I=1,27)/ 'A','B','C','D','E','F','G','H','I',
     X                         'J','K','L','M','N','O','P','Q','R',
     X                         'S','T','U','V','W','X','Y','Z','-'/
      DATA (ISYM(I,2),I=1,27)/ 'A','B','C','D','E','F','G','H','I',
     X                         'J','K','L','M','N','O','P','Q','R',
     X                         'S','T','U','V','W','X','Y','Z','+'/
      DATA ILWR/ 32 /
      DO 10 I=1,26
         L=ICHAR(ISYM(I,2))
         ISYM(I,1)=CHAR(L+ILWR)
   10 CONTINUE
      RETURN
      END
