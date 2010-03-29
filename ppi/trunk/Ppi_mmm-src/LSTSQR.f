c
c----------------------------------------------------------------------X
c
      SUBROUTINE LSTSQR(M,N,X,Y,C,TX)
C
C     LEAST-SQUARES POLYNOMIAL CURVE FIT (LSTSQR)  PAUL SWARZTRAUBER
C
C     DEFINITION OF INPUT PARAMETERS
C
C     M IS THE NUMBER OF COEFFICIENTS DESIRED
C     N IS THE NUMBER OF POINTS TO BE FITTED
C     X IS THE LOCATION OF THE INDEPENDENT VARIABLE TABLE
C     Y IS THE LOCATION OF THE    DEPENDENT VARIABLE TABLE
C     C IS THE LOCATION OF THE COMPUTED COEFFICIENTS
C     C(1) IS THE CONSTANT COEFFICIENT OF THE POLYNOMIAL
C     TX MUST BE DIMENSIONED AT LEAST N
C        AND IS A WORKSPACE ARRAY.
C
      DIMENSION A(20,20)
      DIMENSION C(10),X(1),Y(1),TX(1)
      IF(M.EQ.1)GO TO 60
      DO 20 I=1,N
20    TX(I)=1.0
      DO 30 J=1,M
      A(1,J)=0.
      C(J)=0.
      DO 30 I=1,N
      A(1,J)=A(1,J)+TX(I)
      C(J)=C(J)+TX(I)*Y(I)
30    TX(I)=TX(I)*X(I)
      DO 40 J=2,M
      A(J,M)=0.
      DO 40 I=1,N
      A(J,M)=A(J,M)+TX(I)
40    TX(I)=TX(I)*X(I)
      DO 50 I=2,M
      DO 50 J=2,M
50    A(I,J-1)=A(I-1,J)
      CALL EQSLV (M,1,A,C,D)
      RETURN
   60 CONTINUE
      SUM=0.0
      DO 65 I=1,N
      SUM=SUM+Y(I)
   65 CONTINUE
      C(1)=SUM/N
      C(2)=0.0
      RETURN
      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE EQSLV(N,MS,A,B,D)
C
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
c
c----------------------------------------------------------------------X
c
      FUNCTION CORR (N,X,Y,SE)
C
C        CALCULATES THE CORRELATION COOEFFICIENT OF A DATA SERIES
C
C
C         N- NUMBER OF DATA POINTS
C         X- ABCISSA VALUES
C         Y- ORDINATE VALUES
C        SE- STANDARD ERROR OF ESTIMATE
C
      DOUBLE PRECISION AMN1,AMN2,SUM1,SUM2,SUM3,SSQR1,SSQR2,
     X                 VAR1,VAR2
      DIMENSION X(1), Y(1)
      SUM1=0.0
      SUM2=0.0
      SUM3=0.0
      SSQR1=0.0
      SSQR2=0.0
      IF(N.LE.0) GO TO 50
      FN=1./N
      DO 40 I=1,N
         SUM1=SUM1+X(I)
         SUM2=SUM2+Y(I)
         SUM3=SUM3+X(I)*Y(I)
         SSQR1=SSQR1+X(I)*X(I)
         SSQR2=SSQR2+Y(I)*Y(I)
   40 CONTINUE
      IF(SSQR1.LE.0.0.OR.SSQR2.LE.0.0) GO TO 50
      AMN1=SUM1*FN
      AMN2=SUM2*FN
      VAR1=SSQR1*FN-(AMN1*AMN1)
      VAR2=SSQR2*FN-(AMN2*AMN2)
      IF (VAR1.LE.0.0 .OR. VAR2.LE.0.0) GOTO 50
      CORR=((SUM3*FN)-(AMN1*AMN2))/SQRT(VAR1*VAR2)
      IF ((VAR2*(1.0-CORR*CORR)).LT.0.0) GOTO 50
      SE=SQRT(VAR2*(1.0-CORR*CORR))
      RETURN
   50 CONTINUE
C
C        NO DATA- ERROR EXIT
C
      CORR=0.0
      SE=0.0
      RETURN
      END
c
c----------------------------------------------------------------------X
c
      FUNCTION FUNK(YVAL,CF,NCF)
C
C        EVALUATES A POLYNOMIAL FUNCTION WHOSE COOEFFICIENTS ARE
C                  GIVEN IN CF (ORDER= NCF-1) AT ORDINATE VALUE=YVAL
C
      DIMENSION CF(NCF)
      FUNK=CF(1)
      IF(NCF.LE.1) RETURN
      DO 10 I=2,NCF
         IM1=I-1
         FUNK=FUNK+CF(I)*YVAL**IM1
   10 CONTINUE
      RETURN
      END
