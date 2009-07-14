      SUBROUTINE SINSRT(A,II,JJ)
C
C  SORTS ARRAY A INTO INCREASING ORDER, FROM A(II) TO A(JJ)
C  ORDERING IS BY INTEGER SUBTRACTION, THUS FLOATING POINT
C  NUMBERS MUST BE IN NORMALIZED FORM.
C  ARRAYS IU(K) AND IL(K) PERMIT SORTING UP TO 2**(K+1)-1 ELEMENTS
C  Note: If the maximum number of grid points is 511 x 511 = 261121
C        then MBITS = 17 to allocate sufficient memory for sorting.
C
      PARAMETER (MBITS=17)
      INTEGER   A, T, TT
      DIMENSION A(JJ),IU(MBITS),IL(MBITS)

      NMAX=2**(MBITS+1)-1
      IF(JJ.GT.NMAX)THEN
         PRINT *,'SINSRT ERROR: ',JJ,' INPUT ELEMENTS EXCEEDS ',
     x        NMAX,', THE MAXIMUM THAT CAN BE SORTED.'
         STOP
      ELSE
         PRINT *,'SINSRT  can sort ', NMAX,' grid values by azimuth'
         PRINT *,'SINSRT will sort NX by NY values=',JJ
      END IF

      M=1
      I=II
      J=JJ
    5 IF(I .GE. J) GO TO 70
   10 K=I
      IJ=(J+I)/2
      T=A(IJ)
      IF(A(I) .LE. T) GO TO 20
      A(IJ)=A(I)
      A(I)=T
      T=A(IJ)
   20 L=J
      IF(A(J) .GE. T) GO TO 40
      A(IJ)=A(J)
      A(J)=T
      T=A(IJ)
      IF(A(I) .LE. T) GO TO 40
      A(IJ)=A(I)
      A(I)=T
      T=A(IJ)
      GO TO 40
   30 A(L)=A(K)
      A(K)=TT
   40 L=L-1
      IF(A(L) .GT. T)GO TO 40
      TT=A(L)
   50 K=K+1
      IF(A(K) .LT. T) GO TO 50
      IF(K .LE. L) GO TO 30
      IF(L-I .LE. J-K) GO TO 60
      IL(M)=I
      IU(M)=L
      I=K
      M=M+1
      GO TO 80
   60 IL(M)=K
      IU(M)=J
      J=L
      M=M+1
      GO TO 80
   70 M=M-1
      IF(M .EQ. 0) RETURN
      I=IL(M)
      J=IU(M)
   80 IF(J-I .GE. 11) GO TO 10
      IF(I .EQ. II) GO TO 5
      I=I-1
   90 I=I+1
      IF(I .EQ. J) GO TO 70
      T=A(I+1)
      IF(A(I) .LE. T) GO TO 90
      K=I
  100 A(K+1)=A(K)
      K=K-1
      IF(T .LT. A(K)) GO TO 100
      A(K+1)=T
      GO TO 90
      END






