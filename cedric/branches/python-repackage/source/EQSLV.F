      SUBROUTINE EQSLV(N,MS,A,B,D)
C     N IS THE NUMBER OF EQUATIONS
C     MS IS THE NUMBER OF SOLUTION VECTORS
C     A IS THE COEFFICIENT MATRIX OF ORDER N BY N  IT IS DESTROYED
C     B IS THE RIGHT HAND SIDE OF THE EQUATIONS ,AN N BY MS MATRIX
C     B IS REPLACED BY THE SOLUTION MATRIX
C     D IS THE DETERMINANT OF A
C     THE DIMENSIONS OF A AND B MUST BE THE SAME AS IN THE MAIN PROGRAM
      DIMENSION A(20,20),B(20,20),JH(20)
      D=1.
      KK=N-1
      IF(KK.EQ.0) GO TO 18
      NP=N+1
      DO 16 M=1,KK
      MP=M+1
      AMAX = 0.
      DO 1 J=M,N
      DO 1 I=M,N
      AE = ABS(A(I,J))
      IF (AMAX-AE) 2,1,1
    2 AMAX = AE
      K=I
      L=J
1     CONTINUE
      JH(M)=L
      IF(K-M)5,6,5
5     D=-D
      DO 3 J=1,N
      HOLD=A(K,J)
      A(K,J)=A(M,J)
3     A(M,J)=HOLD
      DO 14 J=1,MS
      HOLD=B(K,J)
      B(K,J)=B(M,J)
14    B(M,J)=HOLD
6     IF(L-M)7,8,7
7     D=-D
      DO 4 I=1,N
      HOLD=A(I,L)
      A(I,L)=A(I,M)
4     A(I,M)=HOLD
8     IF (A(M,M).LT. 1.0E-06) THEN
         A(N,N)=1.0E-07
         GOTO 23
      ELSE
         C1=1./A(M,M)
      END IF
      DO 9 J=MP,N
9     A(M,J)=A(M,J)*C1
      DO 15 J=1,MS
15    B(M,J)=B(M,J)*C1
      DO 11 I=MP,N
      DO 24 J=MP,N
24    A(I,J)=A(I,J)-A(I,M)*A(M,J)
      DO 11 J=1,MS
11    B(I,J)=B(I,J)-A(I,M)*B(M,J)
16    D=D*A(M,M)
18    D=D*A(N,N)
 23   DO 13 L=1,MS
         IF (A(N,N).LT.1.0E-06) THEN
            B(N,L)=999999.99
         ELSE
            B(N,L)=B(N,L)/A(N,N)
         END IF
 13   CONTINUE
      IF(KK.EQ.0) RETURN
      DO 17 L=1,MS
      DO 17 I=1,KK
      J=N-I
      DO 17 K=J,KK
17    B(J,L)=B(J,L)-A(J,K+1)*B(K+1,L)
      DO 19 L=1,KK
      M=N-L
      K=JH(M)
      IF(K-M)20,19,20
20    DO 22 J=1,MS
      HOLD=B(K,J)
      B(K,J)=B(M,J)
22    B(M,J)=HOLD
19    CONTINUE
      RETURN
      END
