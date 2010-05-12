      FUNCTION LOCINT(IFIND,ITAB,N1,N2,LOOK)
C
C     THIS FUNCTION SEARCHES ARRAY ITAB FOR AN ENTRY EQUAL TO
C     IFIND. 
C
C     IFIND- INTEGER BEING SEARCHED FOR
C     LTAB - SEARCH TABLE
C     N1   - FIRST DIMENSION OF TABLE
C     N2   - SECOND DIMENSION OF TABLE
C     LOOK - DIMENSION OF IFIND
C
      DIMENSION IFIND(LOOK),ITAB(N1,N2)
      IF (N2.LE.0) GO TO 15
      DO 10 M=1,N2    
      DO 5 L=1,LOOK
         IF(IFIND(L).NE.ITAB(L,M)) GO TO 10
    5 CONTINUE
      K=M
      GO TO 20
   10 CONTINUE
   15 K=0
   20 CONTINUE
      LOCINT=K
      RETURN
      END